module TyVarMap = Map.Make(SimpleType.TyVar)

type t =
  { term : CompactType.t
  ; rec_vars : CompactType.t TyVarMap.t
  }
