module TyVarMap : module type of Map.Make(SimpleType.TyVar)

type t =
  { term : CompactType.t
  ; rec_vars : CompactType.t TyVarMap.t
  }

val pp : Format.formatter -> t -> unit
