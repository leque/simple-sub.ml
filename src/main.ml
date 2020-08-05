let simplify_ty name ty =
  Logs.debug (fun m -> m "typed `%s` as: %a" name TypeScheme.pp ty);
  let inst = TypeScheme.instantiate 0 ty in
  Logs.debug (fun m -> m "bounds: %a" SimpleType.pp_bounds inst);
  let ct = Typer.compact_type inst in
  Logs.debug (fun m -> m "compact type before simplification: %a" CompactTypeScheme.pp ct);
  let sct = Typer.simplify_compact_type ct in
  Logs.debug (fun m -> m "compact type after simplification: %a" CompactTypeScheme.pp sct);
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

let type_file path =
  let in_ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in in_ch)
    (fun () ->
       let lexbuf = Lexing.from_channel in_ch in
       let parsed = Parser.program Lexer.token lexbuf in
       let tys, _ctx = Typer.type_top parsed in
       tys |> List.iter (fun (name, ty) ->
           let _, _, _, et = simplify_ty name ty in
           Format.printf "val %s : %a\n\n" name Type.pp et
         ))

let repl () =
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
            let _, _, _, et = simplify_ty name ty in
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

let set_debug () =
  Logs.set_level @@ Some Logs.Debug

let () =
  Logs.set_reporter @@ Logs.format_reporter ();
  begin match Sys.argv with
  | [| _ |] ->
    repl ()
  | [| _; "--debug" |] ->
    set_debug ();
    repl ()
  | [| _; path |] ->
    type_file path
  | [| _; "--debug"; path |] ->
    set_debug ();
    type_file path
  | _ -> assert false
  end;
  exit 0
