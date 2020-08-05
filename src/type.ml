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
  if b then
      Format.fprintf ppf "(";
  f ();
  if b then
      Format.fprintf ppf ")"

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
        if i > 0 then
            Format.fprintf ppf "; ";
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
