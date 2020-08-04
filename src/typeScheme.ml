module TyVarTbl = Hashtbl.Make(SimpleType.TyVar)

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
            let nr = SimpleType.fresh_tv lvl in
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
