module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module SimpleTypeTbl = Hashtbl.Make(SimpleType)
module TyVarMap = Map.Make(SimpleType.TyVar)
module TyVarSet = Set.Make(SimpleType.TyVar)
module TyVarTbl = Hashtbl.Make(SimpleType.TyVar)
module PolarVarSet = Set.Make(PolarVar)

module Ref = U.Ref
module TyVar = SimpleType.TyVar

type ctx = TypeScheme.t StringMap.t

let bool_type = SimpleType.primitive "bool"

let int_type = SimpleType.primitive "int"

let builtins =
  let open StringMap in
  let v = SimpleType.fresh_ty_var 1 in
  empty
  |> add "true" @@ TypeScheme.simple bool_type
  |> add "false" @@ TypeScheme.simple bool_type
  |> add "not" @@ TypeScheme.simple SimpleType.O.(bool_type @-> bool_type)
  |> add "succ" @@ TypeScheme.simple SimpleType.O.(int_type @-> int_type)
  |> add "if" @@ TypeScheme.poly 0 @@ SimpleType.O.(bool_type @-> v @-> v @-> v)

let extrude ty polarity lvl =
  let open SimpleType in
  let cache = TyVarTbl.create 16 in
  let rec recur ty polarity lvl =
    Logs.debug (fun m ->
        m "extrude: (%a) %a %d" SimpleType.pp ty Polarity.pp polarity lvl);
    if level ty <= lvl then
      ty
    else
      match ty with
      | Arrow (_, l, r) ->
        arrow (recur l (Polarity.negate polarity) lvl)
              (recur r polarity lvl)
      | Record (_, fs) ->
        record (fs |> List.map (fun (n, t) -> (n, recur t polarity lvl)))
      | Primitive _ as t ->
        t
      | TyVar v ->
        begin match TyVarTbl.find_opt cache v with
          | Some v -> v
          | None ->
            let nr = fresh_tv lvl in
            let nv = ty_var nr in
            TyVarTbl.add cache v nv;
            begin match polarity with
              | Positive ->
                v.upper_bounds <- nv :: v.upper_bounds;
                nr.lower_bounds <- v.lower_bounds |> List.map (fun b -> recur b polarity lvl)
              | Negative ->
                v.lower_bounds <- nv :: v.lower_bounds;
                nr.upper_bounds <- v.upper_bounds |> List.map (fun b -> recur b polarity lvl)
            end;
            nv
        end
  in recur ty polarity lvl

module ConstrainCache = Hashtbl.Make(struct
    type t = SimpleType.t * SimpleType.t

    let equal (t11, t12) (t21, t22) =
      SimpleType.equal t11 t21 && SimpleType.equal t12 t22

    let hash (t1, t2) =
      Hashtbl.hash (SimpleType.hash t1, SimpleType.hash t2)
  end)

let constrain lhs rhs =
  let open SimpleType in
  let cache = ConstrainCache.create 16 in
  let rec recur lhs rhs =
    Logs.debug (fun m ->
        m "constrain: %a <: %a" SimpleType.pp lhs SimpleType.pp rhs);
    if lhs == rhs then
      ()
    else begin
      let in_cache =
        match lhs, rhs with
        | (TyVar _), _
        | _, (TyVar _) ->
          if ConstrainCache.mem cache (lhs, rhs) then
            true
          else begin
            ConstrainCache.add cache (lhs, rhs) true;
            false
          end
        | _ -> false
      in
      if not in_cache then begin
        match lhs, rhs with
        | Arrow (_, l0, r0), Arrow (_, l1, r1) ->
          recur l1 l0;
          recur r0 r1
        | Record (_, fs0), Record (_, fs1) ->
          fs0 |> List.iter begin fun (n0, t0) ->
            match List.find_opt (fun (n1, _) -> n1 == n0) fs1 with
            | Some (_, t1) -> recur t0 t1
            | None -> failwith @@ Printf.sprintf "missing field: %s" n0
          end
        | (TyVar v as lhs), rhs when level lhs >= level rhs ->
          v.upper_bounds <- rhs :: v.upper_bounds;
          v.lower_bounds |> List.iter (fun b -> recur b rhs)
        | lhs, (TyVar v as rhs) when level lhs <= level rhs ->
          v.lower_bounds <- lhs :: v.lower_bounds;
          v.upper_bounds |> List.iter (fun b -> recur lhs b)
        | (TyVar _ as lhs), rhs0 ->
          let rhs = extrude rhs0 Polarity.Negative (level lhs) in
          recur lhs rhs
        | lhs0, (TyVar _ as rhs) ->
          let lhs = extrude lhs0 Polarity.Positive (level rhs) in
          recur lhs rhs
        | _, _ ->
          failwith @@ Format.asprintf "cannot constrain: %a <: %a"
            SimpleType.pp lhs
            SimpleType.pp rhs
      end
    end
  in
  recur lhs rhs

let rec type_term term ctx lvl =
  Logs.debug (fun m -> m "type_term: %s" @@ Term.show term);
  match term with
  | Term.Var name ->
    begin match StringMap.find_opt name ctx with
      | Some t ->
        TypeScheme.instantiate lvl t
      | None ->
        failwith @@ Printf.sprintf "identifier not found: %s" name
    end
  | Lam (name, body) ->
    let param_ty = SimpleType.fresh_ty_var lvl in
    let ctx = StringMap.add name (TypeScheme.simple param_ty) ctx in
    let body_ty = type_term body ctx lvl in
    SimpleType.arrow param_ty body_ty
  | App (f, a) ->
    let res_ty = SimpleType.fresh_ty_var lvl in
    let f_ty = type_term f ctx lvl in
    let a_ty = type_term a ctx lvl in
    constrain f_ty @@ SimpleType.arrow a_ty res_ty;
    res_ty
  | Int _ ->
    int_type
  | Selection (r, name) ->
    let res_ty = SimpleType.fresh_ty_var lvl in
    let r_ty = type_term r ctx lvl in
    constrain r_ty @@ SimpleType.record [(name, res_ty)];
    res_ty
  | Record fs ->
    SimpleType.record (fs |> List.map (fun (n, t) -> (n, type_term t ctx lvl)))
  | LetIn (is_rec, name, rhs, body) ->
    let name_ty = type_let_rhs is_rec name rhs ctx lvl in
    let ctx = StringMap.add name name_ty ctx in
    type_term body ctx lvl
and type_let_rhs is_rec name rhs ctx lvl =
  let res =
    if is_rec then begin
      let e_ty = SimpleType.fresh_ty_var @@ lvl + 1 in
      let ctx = StringMap.add name (TypeScheme.simple e_ty) ctx in
      let rhs_ty = type_term rhs ctx @@ lvl + 1 in
      constrain rhs_ty e_ty;
      e_ty
    end else
      type_term rhs ctx @@ lvl + 1
  in
  TypeScheme.poly lvl res

let type_top ?(ctx = builtins) decls =
  let typs, ctx =
    List.fold_left (fun (typs, ctx) (is_rec, name, term) ->
        let ty = type_let_rhs is_rec name term ctx 0 in
        (name, ty)::typs, StringMap.add name ty ctx)
      ([], ctx) decls
  in (List.rev typs, ctx)

module PolarVarTbl = Hashtbl.Make(PolarVar)

let tv_of_tyvar { SimpleType.uid; _ } =
  { Type.name_hint = "a"; uid }

let expand_simple_type st =
  let recursive = PolarVarTbl.create 16 in
  let rec go st polarity in_process =
    match st with
    | SimpleType.Arrow (_, l, r) ->
      Type.Arrow ( go l (Polarity.negate polarity) in_process
                 , go r polarity in_process
                 )
    | SimpleType.Record (_, fs) ->
      Type.Record (fs |> List.map (fun (n, t) -> (n, go t polarity in_process)))
    | SimpleType.Primitive n ->
      Type.Primitive n
    | SimpleType.TyVar var ->
      let pv = { PolarVar.polarity; var } in
      if PolarVarSet.mem pv in_process then
        begin match PolarVarTbl.find_opt recursive pv with
          | Some t -> Type.TyVar t
          | None ->
            let r = tv_of_tyvar (SimpleType.fresh_tv 0) in
            PolarVarTbl.add recursive pv r;
            Type.TyVar r
        end
      else begin
        let bounds, f =
          match polarity with
          | Positive ->
            var.lower_bounds, (fun x y -> Type.Union (x, y))
          | Negative ->
            var.upper_bounds, (fun x y -> Type.Intersection (x, y))
        in
        let bound_types =
          bounds |> List.map (fun t -> go t polarity (PolarVarSet.add pv in_process))
        in
        let res = bound_types |> List.fold_left f (Type.TyVar (tv_of_tyvar var)) in
        match PolarVarTbl.find_opt recursive pv with
        | Some t -> Type.Rec (t, res)
        | None -> res
      end
  in
  go st Polarity.Positive PolarVarSet.empty

let compact_type st =
  let recursive = PolarVarTbl.create 16 in
  let rec_vars = ref TyVarMap.empty in
  let rec go st polarity parents in_process =
    match st with
    | SimpleType.Primitive n ->
      CompactType.create ~prims:(StringSet.singleton n)
        ()
    | SimpleType.Arrow (_, l, r) ->
      CompactType.create ~fun_:( go l (Polarity.negate polarity) TyVarSet.empty in_process
                               , go r polarity TyVarSet.empty in_process
                               )
        ()
    | SimpleType.Record (_, fs) ->
      let record =
        fs
        |> List.to_seq
        |> Seq.map (fun (n, t) -> (n, go t polarity TyVarSet.empty in_process))
        |> StringMap.of_seq
      in
      CompactType.create ~record ()
    | TyVar var ->
      let bounds =
        match polarity with
        | Positive -> var.lower_bounds
        | Negative -> var.upper_bounds
      in
      let pv = { PolarVar.var; polarity } in
      if PolarVarSet.mem pv in_process then begin
        if TyVarSet.mem var parents then
          CompactType.create ()
        else
          let v =
            match PolarVarTbl.find_opt recursive pv with
            | Some tv -> tv
            | None ->
              let res = SimpleType.fresh_tv 0 in
              PolarVarTbl.add recursive pv res;
              res
          in
          let vars = TyVarSet.singleton v in
          CompactType.create ~vars ()
      end else
        let in_process = in_process |> PolarVarSet.add pv in
        let bound =
          bounds
          |> List.map (fun b -> go b polarity (TyVarSet.add var parents) in_process)
          |> List.fold_left
            (CompactType.merge polarity)
            (CompactType.create ~vars:(TyVarSet.singleton var) ())
        in
        match PolarVarTbl.find_opt recursive pv with
        | Some v ->
          Ref.replace rec_vars ~f:(TyVarMap.add v bound);
          CompactType.create ~vars:(TyVarSet.singleton v) ()
        | None -> bound
  in
  { CompactTypeScheme.term = go st Polarity.Positive TyVarSet.empty PolarVarSet.empty
  ; rec_vars = !rec_vars
  }

module RevTyVarSet = Set.Make(struct
    include TyVar
    let compare x y = -(compare x y)
  end)

let simplify cty =
  let all_vars =
    RevTyVarSet.empty
    |> RevTyVarSet.add_seq (TyVarMap.to_seq cty.CompactTypeScheme.rec_vars |> Seq.map fst)
    |> ref
  in
  let rec_vars = ref TyVarMap.empty in
  let co_occs = PolarVarTbl.create 16 in
  let var_subst = TyVarTbl.create 16 in
  let rec go ty polarity : unit -> CompactType.t =
    ty.CompactType.vars |> TyVarSet.iter begin fun tv ->
      Ref.replace all_vars ~f:(RevTyVarSet.add tv);
      let new_occs = SimpleTypeTbl.create 16 in
      let put_seq s = s |> Seq.map (fun x -> x, true) |> SimpleTypeTbl.add_seq new_occs in
      put_seq (ty.vars |> TyVarSet.to_seq |> Seq.map (fun tv -> SimpleType.ty_var tv));
      put_seq (ty.prims |> StringSet.to_seq |> Seq.map SimpleType.primitive);
      let pv = { PolarVar.var = tv; polarity } in
      begin match PolarVarTbl.find_opt co_occs pv with
        | None ->
          PolarVarTbl.add co_occs pv new_occs
        | Some os ->
          os |> SimpleTypeTbl.filter_map_inplace (fun k v ->
              if SimpleTypeTbl.mem new_occs k then
                Some v
              else
                None)
      end;
      begin match TyVarMap.find_opt tv cty.CompactTypeScheme.rec_vars with
        | None -> ()
        | Some b ->
          if not @@ TyVarMap.mem tv !rec_vars then begin
            let rec go_later () =
              Ref.replace rec_vars ~f:(TyVarMap.add tv go_later);
              go b polarity ()
            in
            ignore @@ go_later ()
          end
      end;
    end;
    let record =
      ty.record
      |> Option.map (fun r -> r |> StringMap.map (fun x -> go x polarity))
    in
    let fun_ =
      ty.fun_
      |> Option.map (fun (l, r) -> (go l (Polarity.negate polarity), go r polarity))
    in
    (fun () ->
       let new_vars = TyVarSet.fold (fun tv acc ->
           match TyVarTbl.find_opt var_subst tv with
           | Some (Some tv2) ->
             TyVarSet.add tv2 acc
           | Some (None) ->
             acc
           | None ->
             TyVarSet.add tv acc)
           ty.vars
           TyVarSet.empty
       in
       CompactType.create
         ~vars:new_vars
         ~prims:ty.prims
         ?record:(record |> Option.map (fun r -> r |> StringMap.map (fun v -> v ())))
         ?fun_:(fun_ |> Option.map (fun (l, r) -> (l (), r ())))
         ()
    )
  in
  let gone = go cty.term Polarity.Positive in
  !all_vars |> RevTyVarSet.iter begin fun v0 ->
    if not @@ TyVarMap.mem v0 !rec_vars then begin
      let pos = { PolarVar.var = v0; polarity = Polarity.Positive } in
      let neg = { PolarVar.var = v0; polarity = Polarity.Negative } in
      match PolarVarTbl.find_opt co_occs pos, PolarVarTbl.find_opt co_occs neg with
      | Some _, None
      | None, Some _ ->
        TyVarTbl.add var_subst v0 None
      | Some _, Some _ -> ()
      | None, None -> assert false
    end
  end;
  !all_vars |> RevTyVarSet.iter begin fun v ->
    if not @@ TyVarTbl.mem var_subst v then begin
      Polarity.values |> List.iter begin fun pol ->
        let pv = { PolarVar.var = v; polarity = pol } in
        PolarVarTbl.find_opt co_occs pv
        |> Option.map SimpleTypeTbl.to_seq_keys
        |> Option.value ~default:Seq.empty
        |> Seq.iter begin function
          | SimpleType.TyVar w
            when w != v
              && not (TyVarTbl.mem var_subst w)
              && TyVarMap.mem v !rec_vars = TyVarMap.mem w !rec_vars ->
            let b = PolarVarTbl.find_opt co_occs pv
                    |> Option.map (fun tbl -> SimpleTypeTbl.mem tbl (SimpleType.ty_var v))
                    |> Option.value ~default:true
            in
            if b then begin
              TyVarTbl.add var_subst w (Some v);
              match TyVarMap.find_opt w !rec_vars with
              | Some bw ->
                Ref.replace rec_vars ~f:(TyVarMap.remove w);
                let bv = TyVarMap.find v !rec_vars in
                Ref.replace rec_vars
                  ~f:(TyVarMap.add v (fun () -> CompactType.merge pol (bv ()) (bw ())))
              | None ->
                let neg_wpv = { PolarVar.var = w; polarity = Polarity.negate pol } in
                let w_co_occs = PolarVarTbl.find co_occs neg_wpv in
                let neg_vpv = { PolarVar.var = v; polarity = Polarity.negate pol } in
                SimpleTypeTbl.filter_map_inplace (fun t x ->
                    if t == SimpleType.ty_var v || SimpleTypeTbl.mem w_co_occs t then
                      Some x
                    else
                      None)
                  (PolarVarTbl.find co_occs neg_vpv)
            end
          | SimpleType.Primitive _
            when PolarVarTbl.find_opt co_occs { PolarVar.var = v; polarity = Polarity.negate pol }
                 |> Option.map (fun tbl -> SimpleTypeTbl.mem tbl (SimpleType.ty_var v))
                 |> Option.value ~default:false
            ->
            TyVarTbl.add var_subst v None
          | _ ->
            ()
        end
      end
    end
  end;
  { CompactTypeScheme.term = gone ()
  ; rec_vars = !rec_vars |> TyVarMap.map (fun f -> f ())
  }

module CtyOrVar = struct
  type t =
    | Cty of CompactType.t
    | TyVar of TyVar.t
  [@@deriving eq, ord]
end

module PCty = struct
  type t = { ty : CtyOrVar.t; polarity : Polarity.t }
  [@@deriving eq, ord]
end

module PCtyMap = Map.Make(PCty)

let expand_compact_type cty =
  let rec go ty polarity in_process : Type.t =
    match PCtyMap.find_opt { ty; polarity } in_process with
    | Some t ->
      Type.TyVar (Lazy.force t)
    | None ->
      let is_recursive = ref false in
      let v = lazy begin
        is_recursive := true;
        let t =
          match ty with
          | CtyOrVar.TyVar tv -> tv
          | _ -> SimpleType.fresh_tv 0
        in tv_of_tyvar t
      end
      in
      let in_process = PCtyMap.add { ty; polarity } v in_process in
      let res =
        match ty with
        | CtyOrVar.TyVar tv ->
          begin match TyVarMap.find_opt tv cty.CompactTypeScheme.rec_vars with
            | None ->
              Type.TyVar (tv_of_tyvar tv)
            | Some t ->
              go_cty t polarity in_process
          end
        | CtyOrVar.Cty { vars; prims; record; fun_ } ->
          let extr, merge =
            match polarity with
            | Polarity.Positive ->
              Type.Bot, (fun x y -> Type.Union (x, y))
            | Polarity.Negative ->
              Type.Top, (fun x y -> Type.Intersection (x, y))
          in
          let vs = vars
                   |> TyVarSet.to_seq
                   |> Seq.map (fun v -> go (CtyOrVar.TyVar v) polarity in_process)
                   |> List.of_seq
          in
          let ps = prims
                   |> StringSet.to_seq
                   |> Seq.map (fun n -> Type.Primitive n)
                   |> List.of_seq
          in
          let rs = record
                   |> Option.map (fun fs ->
                       fs
                       |> StringMap.to_seq
                       |> Seq.map (fun (n, t) -> (n, go_cty t polarity in_process))
                       |> List.of_seq
                       |> (fun x -> Type.Record x))
                   |> Option.to_list
          in
          let fs = fun_
                   |> Option.map (fun (l, r) ->
                       Type.Arrow
                         ( go_cty l (Polarity.negate polarity) in_process
                         , go_cty r polarity in_process))
                   |> Option.to_list
          in
          match vs @ ps @ rs @ fs with
          | [] -> extr
          | x :: xs ->
            List.fold_left merge x xs
      in
      if !is_recursive then
        Type.Rec (Lazy.force v, res)
      else
        res
  and go_cty t polarity in_process = go (CtyOrVar.Cty t) polarity in_process
  in go_cty cty.term Polarity.Positive PCtyMap.empty
