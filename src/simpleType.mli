type t =
  | Arrow of int * t * t
  | Record of int * (string * t) list
  | Primitive of string
  | TyVar of tv
and tv =
  { level : int
  ; uid : int
  ; mutable lower_bounds : t list
  ; mutable upper_bounds : t list
  }

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string

val equal_tv : tv -> tv -> bool
val compare_tv : tv -> tv -> int
val pp_tv : Format.formatter -> tv -> unit

module TyVar : sig
  type t = tv
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

val pp_bounds : Format.formatter -> t -> unit

val level : t -> int
val fresh_tv : int -> tv
val arrow : t -> t -> t
val record : (string * t) list -> t
val primitive : string -> t
val fresh_ty_var : int -> t

module O : sig
  val ( @-> ) : t -> t -> t
end
