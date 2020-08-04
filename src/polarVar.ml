module TyVar = SimpleType.TyVar

type t =
  { var : TyVar.t
  ; polarity : Polarity.t
  }
[@@deriving eq, ord]

let hash t =
  Hashtbl.hash (TyVar.hash t.var, t.polarity)
