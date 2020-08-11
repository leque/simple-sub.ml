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
and tv = { name_hint : string; uid : int; }

val pp : Format.formatter -> t -> unit
val show : t -> string
