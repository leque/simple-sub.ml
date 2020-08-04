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

let fresh_tv =
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

let ty_var tv =
  TyVar tv

let fresh_ty_var lvl =
  TyVar (fresh_tv lvl)

module O = struct
  let (@->) = arrow
end
