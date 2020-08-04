let f t =
  let typ = Typer.type_term t Typer.builtins 0 in
  let ty = Typer.expand_simple_type typ in
  let cty = Typer.compact_type typ in
  let cty = Typer.simplify cty in
  let ety = Typer.expand_compact_type cty in
  ignore ty;
  Format.printf "inferred: %a\n" Type.pp ty;
  Format.printf "bounds: %a\n" SimpleType.pp_bounds typ;
  Format.printf "simplified: %a\n\n" Type.pp ety;
  ()

let () =
  Logs.set_reporter (Logs.format_reporter ())
  ; Logs.set_level (Some Logs.Debug)
  ; f Term.O.(let_in "f" (lamb "x" (v"x" $ v"x"))
              (v"f"))
  ; f Term.O.(v "if" $ v "true" $ i 1 $ i 2 )
  ; f Term.O.(let_in "f" (lamb "x"
                            (lamb "y"
                               (v"if" $ v"x" $ (v"y").*("foo") $ (v"y").*("bar"))))
                (v"f"))
