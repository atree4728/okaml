let rec read_eval_print env tyenv lexbuf =
  let open Result in
  print_string "# ";
  flush stdout;
  try
    let cmd_opt = Parser.toplevel Lexer.main lexbuf in
    match cmd_opt with
    | None -> ()
    | Some cmd ->
      let continue =
        (* let* tys, newtyenv = Typing.infer_cmd tyenv cmd in *)
        let* prompts, newenv = Eval.eval_command env cmd in
        prompts
        |> List.iter (fun (prompt, value_string) ->
          Printf.printf "%s : (unknown) = %s\n" prompt value_string);
        Ok (read_eval_print newenv Env.empty lexbuf)
        (* List.combine tys prompts *)
        (* |> List.iter (fun (ty, (prompt, value_string)) -> *)
        (*   Printf.printf "%s : %s = %s\n" prompt ty value_string); *)
        (* Ok (read_eval_print newenv newtyenv lexbuf) *)
      in
      Result.fold
        ~ok:Fun.id
        ~error:(fun err -> raise (Failure (Error.string_of_error err)))
        continue
  with
  | Failure msg ->
    print_endline msg;
    read_eval_print env tyenv lexbuf
  | Parser.Error ->
    print_endline "Error: Syntax error";
    read_eval_print env tyenv lexbuf
;;

let _ = read_eval_print Env.empty Env.empty (Lexing.from_channel stdin)
