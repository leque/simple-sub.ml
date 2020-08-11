module StringSet : module type of Set.Make(String)
module StringMap : module type of Map.Make(String)
module TyVarSet : module type of Set.Make(SimpleType.TyVar)

type t =
  { vars : TyVarSet.t
  ; prims : StringSet.t
  ; record : t StringMap.t option
  ; fun_ : (t * t) option
  }

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val create :
  ?vars:TyVarSet.t
  -> ?prims:StringSet.t
  -> ?record:t StringMap.t
  -> ?fun_:t * t
  -> unit
  -> t

val merge : Polarity.t -> t -> t -> t
