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

let simplify_ty ty =
  let inst = TypeScheme.instantiate 0 ty in
  let ct = Typer.compact_type inst in
  let sct = Typer.simplify ct in
  let et = Typer.expand_compact_type sct in
  inst, ct, sct, et

let string_ends_with ~suffix s =
  let rec loop i j =
    if i > j then
      false
    else if i < 0 then
      true
    else
      suffix.[i] = s.[j] && loop (i - 1) (j - 1)
  in loop (String.length suffix - 1) (String.length s - 1)

let () =
  if Array.length Sys.argv > 1 then
    let in_ch = open_in Sys.argv.(1) in
    Fun.protect ~finally:(fun () -> close_in in_ch)
      (fun () ->
         let lexbuf = Lexing.from_channel in_ch in
         let parsed = Parser.program Lexer.token lexbuf in
         let tys, _ctx = Typer.type_top parsed in
         tys |> List.iter (fun (name, ty) ->
             let _, _, _, et = simplify_ty ty in
             Format.printf "val %s : %a\n\n" name Type.pp et
           ))
  else begin
    let prompt () =
      Format.print_string "> ";
      Format.print_flush ();
    in
    let buf = Buffer.create 256 in
    let rec loop ctx =
      match input_line stdin with
      | line ->
        Buffer.add_string buf line;
        if string_ends_with ~suffix:";;" line then begin
          let s = Buffer.contents buf in
          let s = String.sub s 0 (String.length s - 2) in
          let lexbuf = Lexing.from_string s in
          let parsed = Parser.program Lexer.token lexbuf in
          Buffer.clear buf;
          let tys, ctx = Typer.type_top parsed ~ctx in
          tys |> List.iter (fun (name, ty) ->
              let _, _, _, et = simplify_ty ty in
              Format.printf "val %s : %a\n\n" name Type.pp et;
            );
          prompt ();
          loop ctx
        end else
          loop ctx
      | exception End_of_file ->
        ()
    in
    prompt ();
    loop Typer.builtins
  end;
  exit 0

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
