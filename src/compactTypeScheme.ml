module TyVarMap = Map.Make(SimpleType.TyVar)

type t =
  { term : CompactType.t
  ; rec_vars : CompactType.t TyVarMap.t
  }

let pp ppf { term; rec_vars } =
  Format.fprintf ppf "CompactTypeScheme(%a, " CompactType.pp term;
  Format.fprintf ppf "Map(";
  let idx = ref 0 in
  rec_vars |> TyVarMap.iter (fun tv ct ->
      if !idx > 0 then
        Format.fprintf ppf "; ";
      incr idx;
      Format.fprintf ppf "%a -> %a" SimpleType.TyVar.pp tv CompactType.pp ct
    );
  Format.fprintf ppf "))"
