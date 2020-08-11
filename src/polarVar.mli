type t =
  { var : SimpleType.TyVar.t
  ; polarity : Polarity.t
  }

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
