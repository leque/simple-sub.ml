type t = Positive | Negative

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string

val negate : t -> t
val values : t list
