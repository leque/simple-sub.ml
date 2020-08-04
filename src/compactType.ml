module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module TyVarSet = Set.Make(SimpleType.TyVar)

type t =
  { vars : TyVarSet.t
  ; prims : StringSet.t
  ; record : t StringMap.t option
  ; fun_ : (t * t) option
  }
[@@deriving eq, ord]

let create ?(vars = TyVarSet.empty) ?(prims = StringSet.empty) ?record ?fun_ () =
  { vars; prims; record; fun_ }

let empty =
  create ()

let rec merge polarity a b =
  let vars = a.vars |> TyVarSet.add_seq (TyVarSet.to_seq b.vars) in
  let prims = a.prims |> StringSet.add_seq (StringSet.to_seq b.prims) in
  let record = U.merge_opt a.record b.record ~f:begin fun lhs rhs ->
      match polarity with
      | Polarity.Negative ->
        StringMap.merge (fun _ l r -> U.merge_opt ~f:(merge polarity) l r) lhs rhs
      | Positive ->
        StringMap.merge (fun _k l r ->
            match l, r with
            | Some v1, Some v2 ->
              Some (merge polarity v1 v2)
            | _, _ -> None
          ) lhs rhs
    end
  in
  let fun_ = U.merge_opt a.fun_ b.fun_ ~f:(fun (l0, r0) (l1, r1) ->
      merge (Polarity.negate polarity) l0 l1, merge polarity r0 r1)
  in
  { vars; prims; record; fun_ }
