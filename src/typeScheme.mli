module TyVarTbl : module type of Hashtbl.Make(SimpleType.TyVar)

type t

val poly : int -> SimpleType.t -> t

val simple : SimpleType.t -> t

val pp : Format.formatter -> t -> unit

val show : t -> string

val instantiate : int -> t -> SimpleType.t
