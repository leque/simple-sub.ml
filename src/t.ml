module Ref = struct
  type 'a t = 'a ref = { mutable contents : 'a }

  let replace t ~f =
    t := f !t
end

module Term = struct
  type t =
    | Int of int
    | Var of string
    | Lam of string * t
    | App of t * t
    | Record of (string * t) list
    | Selection of t * string
    | LetIn of bool * string * t * t
    [@@deriving show]

  module O = struct
    let i n = Int n

    let v n = Var n

    let lamb name body = Lam (name, body)

    let ($) f a = App (f, a)

    let r fs = Record fs

    let (.*()) r label =
      Selection (r, label)

    let let_in name expr body =
      LetIn (false, name, expr, body)

    let let_rec_in name expr body =
      LetIn (true, name, expr, body)
  end
end

module Type = struct
  type t =
    | Union of t * t
    | Intersection of t * t
    | Arrow of t * t
    | Record of (string * t) list
    | Rec of tv * t
    | Top
    | Bot
    | Primitive of string
    | TyVar of tv
  and tv =
    { name_hint : string
    ; uid : int
    }

  module Var = struct
    type t = tv =
      { name_hint : string
      ; uid : int
      }
      [@@deriving eq, ord]
  end

  module VarMap = Map.Make(Var)

  let rec type_vars = function
    | TyVar tv -> [tv]
    | Rec (tv, b) -> tv :: type_vars b
    | Union (l, r)
    | Intersection (l, r)
    | Arrow (l, r) ->
      type_vars l @ type_vars r
    | Record fs ->
      fs |> List.concat_map (fun (_, t) -> type_vars t)
    | Top | Bot | Primitive _ -> []

  let none_prec = 0
  let arrow_prec = 11
  let union_prec = 20
  let intersection_prec = 25
  let rec_prec = 31

  let maybe_paren ppf b f =
    begin if b then
        Format.fprintf ppf "("
    end;
    f ();
    begin if b then
        Format.fprintf ppf ")"
    end

  let rec pp_in ppf ctx outer_prec = function
    | Top -> Format.fprintf ppf "⊤"
    | Bot -> Format.fprintf ppf "⊥"
    | Primitive name ->
      Format.fprintf ppf "%s" name
    | TyVar tv ->
      Format.fprintf ppf "%s" @@ VarMap.find tv ctx
    | Rec (n, b) ->
      pp_in ppf ctx rec_prec b;
      Format.fprintf ppf " as ";
      Format.fprintf ppf "%s" @@ VarMap.find n ctx
    | Arrow (l, r) ->
      maybe_paren ppf (outer_prec > arrow_prec) (fun () ->
            pp_in ppf ctx (arrow_prec + 1) l;
            Format.fprintf ppf " -> ";
            pp_in ppf ctx arrow_prec r)
    | Record fs ->
      Format.fprintf ppf "{";
      fs |> List.iteri (fun i (n, t) ->
          begin if i > 0 then
            Format.fprintf ppf ", "
          end;
          Format.fprintf ppf "%s: " n;
          pp_in ppf ctx 0 t;
        );
      Format.fprintf ppf "}"
    | Union (l, r) ->
      maybe_paren ppf (outer_prec > union_prec) (fun () ->
          pp_in ppf ctx union_prec l;
          Format.fprintf ppf " ∧ ";
          pp_in ppf ctx union_prec r)
    | Intersection (l, r) ->
      maybe_paren ppf (outer_prec > intersection_prec) (fun () ->
          pp_in ppf ctx intersection_prec l;
          Format.fprintf ppf " ∨ ";
          pp_in ppf ctx intersection_prec r)

  let pp ppf t =
    let vars = type_vars t |> List.sort_uniq Var.compare in
    let ctx =
      vars
      |> List.mapi (fun i v -> v, Printf.sprintf "'%c" @@ Char.chr @@ Char.code 'a' + i)
      |> List.to_seq
      |> (fun x -> VarMap.add_seq x VarMap.empty)
    in
    pp_in ppf ctx none_prec t

  let show t =
    Format.asprintf "%a" pp t
end

module SimpleType = struct
  type t =
    | Arrow of int * t * t
    | Record of int * (string * t) list
    | Primitive of string
    | TyVar of tv
    [@@deriving eq, ord]
  and tv =
    { level : int
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
    ; uid : int
    ; mutable lower_bounds : t list
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
    ; mutable upper_bounds : t list
      [@equal fun _ _ -> true]
      [@compare fun _ _ -> 0]
    }
    [@@deriving eq, ord]

  let pp_tv ppf { uid; level; _ } =
    Format.fprintf ppf "'a%d/%d" uid level

  let pp ppf t =
    let maybe_paren b f =
      begin if b then
        Format.fprintf ppf "("
      end;
      f ();
      begin if b then
        Format.fprintf ppf ")"
      end;
    in
    let none_prec = 0 in
    let fun_prec = 1 in
    let rec recur outer_prec = function
      | Arrow (_, l, r) ->
        maybe_paren (outer_prec > fun_prec) (fun () ->
            recur (fun_prec + 1) l;
            Format.fprintf ppf " -> ";
            recur fun_prec r)
      | Record (_, fs) ->
        Format.fprintf ppf "{";
        fs |> List.iteri (fun i (n, t) ->
           begin if i > 0 then
             Format.fprintf ppf ", "
           end;
           Format.fprintf ppf "%s: " n;
           recur none_prec t
          );
        Format.fprintf ppf "}"
      | Primitive s ->
        Format.fprintf ppf "%s" s
      | TyVar tv ->
        pp_tv ppf tv
    in
    recur none_prec t

  let show t =
    Format.asprintf "%a" pp t

  module TyVar = struct
    type t = tv

    let compare = compare_tv

    let equal = equal_tv

    let hash t = t.uid

    let pp = pp_tv
  end

  let children = function
    | TyVar tv ->
      tv.lower_bounds @ tv.upper_bounds
    | Arrow (_, l, r) ->
      [l; r]
    | Record (_, fs) ->
      fs |> List.map snd
    | Primitive _ ->
      []

  module TyVarSet = Set.Make(TyVar)

  let all_vars t =
    let rec recur acc = function
      | [] -> acc
      | TyVar tv :: rest ->
        if TyVarSet.mem tv acc then
          recur acc rest
        else begin
          recur (TyVarSet.add tv acc) (children t @ rest)
        end
      | ty :: rest ->
        recur acc (children ty @ rest)
    in
    recur TyVarSet.empty [t]

  let pp_bounds ppf t =
    let is_empty = function [] -> true | _ -> false in
    let pr prefix sep ppf bs =
      if not @@ is_empty bs then begin
        Format.fprintf ppf " %s " prefix;
        bs |> List.iteri (fun i b ->
            if i > 0 then
              Format.fprintf ppf " %s " sep;
            Format.fprintf ppf "%a" pp b)
      end
    in
    all_vars t
    |> TyVarSet.to_seq
    |> Seq.filter (fun tv -> not (is_empty tv.lower_bounds) || not (is_empty tv.upper_bounds))
    |> List.of_seq
    |> List.iteri (fun i tv ->
        if i > 0 then
          Format.fprintf ppf ", ";
        Format.fprintf ppf "%a%a%a"
          TyVar.pp tv
          (pr ">:" "|") tv.lower_bounds
          (pr "<:" "&") tv.upper_bounds)

  let rec hash = function
    | Arrow (lv, l, r) ->
      Hashtbl.hash (0, lv, hash l, hash r)
    | Record (lv, fs) ->
      Hashtbl.hash (1, lv, fs |> List.map (fun (n, t) -> (n, hash t)))
    | Primitive name ->
      Hashtbl.hash (2, name)
    | TyVar tv ->
      Hashtbl.hash (3, TyVar.hash tv)

  let level = function
    | Arrow (l, _, _) -> l
    | Record (l, _) -> l
    | Primitive _ -> 0
    | TyVar { level; _ } -> level

  let fresh_var =
    let count = ref 0 in
    fun level ->
      let uid = !count in
      incr count;
      { level
      ; uid
      ; lower_bounds = []
      ; upper_bounds = []
      }

  let arrow l r = Arrow (max (level l) (level r), l, r)

  let record fs =
    let l = fs |> List.fold_left (fun l (_, t) -> max l @@ level t) 0 in
    Record (l, fs)

  let primitive n =
    Primitive n

  let ty_var lvl =
    TyVar (fresh_var lvl)

  module O = struct
    let (@->) = arrow
  end
end

module TyVar = SimpleType.TyVar

module Polarity = struct
  type t = Positive | Negative
  [@@deriving eq, ord, show]

  let negate = function
    | Positive -> Negative
    | Negative -> Positive

  let values = [Positive; Negative]
end

module PolarVar = struct
  type t =
    { var : TyVar.t
    ; polarity : Polarity.t
    }
  [@@deriving eq, ord]

  let hash t =
    Hashtbl.hash (TyVar.hash t.var, t.polarity)
end

module TyVarSet = Set.Make(TyVar)
module TyVarMap = Map.Make(TyVar)
module TyVarTbl = Hashtbl.Make(TyVar)
module PolarVarSet = Set.Make(PolarVar)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module SimpleTypeTbl = Hashtbl.Make(SimpleType)

module TypeScheme = struct
  type t =
    | Poly of int * SimpleType.t
    | Simple of SimpleType.t

  let poly l t = Poly (l, t)

  let simple t = Simple t

  let pp ppf = function
    | Poly (lvl, t) ->
      Format.fprintf ppf "Poly (%d, %a)" lvl SimpleType.pp t
    | Simple t ->
      Format.fprintf ppf "Simple (%a)" SimpleType.pp t

  let show t =
    Format.asprintf "%a" pp t

  let freshen_above ~limit ty lvl =
    let freshened = TyVarTbl.create 16 in
    let open SimpleType in
    let rec freshen t =
      if level t <= limit then
        t
      else
        match t with
        | Arrow (_, l, r) ->
          SimpleType.arrow (freshen l) (freshen r)
        | Record (_, fs) ->
          SimpleType.record (fs |> List.map (fun (n, t) -> (n, freshen t)))
        | Primitive _ as t ->
          t
        | TyVar r ->
          begin match TyVarTbl.find_opt freshened r with
          | Some tv -> tv
          | None ->
            let nr = SimpleType.fresh_var lvl in
            let nv = TyVar nr in
            TyVarTbl.add freshened r nv;
            nr.lower_bounds <- r.lower_bounds |> List.rev_map freshen |> List.rev;
            nr.upper_bounds <- r.upper_bounds |> List.rev_map freshen |> List.rev;
            nv
          end
    in freshen ty

  let instantiate lvl = function
    | Poly (lv, t) ->
      freshen_above ~limit:lvl t lv
    | Simple t -> t
end

module U = struct
  let merge_opt ~f a b =
    match a, b with
    | Some a, Some b -> Some (f a b)
    | Some _, _ -> a
    | _, Some _ -> b
    | _, _ -> None
end

module CompactType = struct
  type t =
    { vars : TyVarSet.t
    ; prims : StringSet.t
    ; record : t StringMap.t option
    ; fun_ : (t * t) option
    }
    [@@deriving eq, ord]

  let create ?(vars = TyVarSet.empty) ?(prims = StringSet.empty) ?record ?fun_ () =
    { vars; prims; record; fun_ }

  let empty =
    create ()

  let rec merge polarity a b =
    let vars = a.vars |> TyVarSet.add_seq (TyVarSet.to_seq b.vars) in
    let prims = a.prims |> StringSet.add_seq (StringSet.to_seq b.prims) in
    let record = U.merge_opt a.record b.record ~f:begin fun lhs rhs ->
        match polarity with
        | Polarity.Negative ->
          StringMap.merge (fun _ l r -> U.merge_opt ~f:(merge polarity) l r) lhs rhs
        | Positive ->
          StringMap.merge (fun _k l r ->
              match l, r with
              | Some v1, Some v2 ->
                Some (merge polarity v1 v2)
              | _, _ -> None
            ) lhs rhs
      end
    in
    let fun_ = U.merge_opt a.fun_ b.fun_ ~f:(fun (l0, r0) (l1, r1) ->
        merge (Polarity.negate polarity) l0 l1, merge polarity r0 r1)
    in
    { vars; prims; record; fun_ }
end

module CompactTypeScheme = struct
  type t =
    { term : CompactType.t
    ; rec_vars : CompactType.t TyVarMap.t
    }
end

module Typer = struct
  type ctx = TypeScheme.t StringMap.t

  let bool_type = SimpleType.primitive "bool"

  let int_type = SimpleType.primitive "int"

  let builtins =
    let open StringMap in
    let v = SimpleType.ty_var 1 in
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
        | Arrow (lv, l, r) ->
          Arrow ( lv
                , recur l (Polarity.negate polarity) lvl
                , recur r polarity lvl
                )
        | Record (lv, fs) ->
          Record (lv, fs |> List.map (fun (n, t) -> (n, recur t polarity lvl)))
        | Primitive _ as t ->
          t
        | TyVar v ->
          begin match TyVarTbl.find_opt cache v with
            | Some v -> v
            | None ->
              let nr = fresh_var lvl in
              let nv = TyVar nr in
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
      let param_ty = SimpleType.ty_var lvl in
      let ctx = StringMap.add name (TypeScheme.simple param_ty) ctx in
      let body_ty = type_term body ctx lvl in
      SimpleType.arrow param_ty body_ty
    | App (f, a) ->
      let res_ty = SimpleType.ty_var lvl in
      let f_ty = type_term f ctx lvl in
      let a_ty = type_term a ctx lvl in
      constrain f_ty @@ SimpleType.arrow a_ty res_ty;
      res_ty
    | Int _ ->
      int_type
    | Selection (r, name) ->
      let res_ty = SimpleType.ty_var lvl in
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
        let e_ty = SimpleType.ty_var @@ lvl + 1 in
        let ctx = StringMap.add name (TypeScheme.simple e_ty) ctx in
        let rhs_ty = type_term rhs ctx @@ lvl + 1 in
        constrain rhs_ty e_ty;
        e_ty
      end else
        type_term rhs ctx @@ lvl + 1
    in
    TypeScheme.poly lvl res

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
            let r = tv_of_tyvar (SimpleType.fresh_var 0) in
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
                let res = SimpleType.fresh_var 0 in
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
      let compare x y = -(TyVar.compare x y)
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
        put_seq (ty.vars |> TyVarSet.to_seq |> Seq.map (fun tv -> SimpleType.TyVar tv));
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
                        |> Option.map (fun tbl -> SimpleTypeTbl.mem tbl (SimpleType.TyVar v))
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
                        if t == SimpleType.TyVar v || SimpleTypeTbl.mem w_co_occs t then
                          Some x
                        else
                          None)
                      (PolarVarTbl.find co_occs neg_vpv)
                end
              | SimpleType.Primitive _
                when PolarVarTbl.find_opt co_occs { PolarVar.var = v; polarity = Polarity.negate pol }
                     |> Option.map (fun tbl -> SimpleTypeTbl.mem tbl (SimpleType.TyVar v))
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
            | _ -> SimpleType.fresh_var 0
          in tv_of_tyvar t
        end
        in
        let in_process = PCtyMap.add { ty; polarity } v in_process in
        let res =
          begin match ty with
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
          end
        in
        if !is_recursive then
          Type.Rec (Lazy.force v, res)
        else
          res
    and go_cty t polarity in_process = go (CtyOrVar.Cty t) polarity in_process
    in go_cty cty.term Polarity.Positive PCtyMap.empty
end
