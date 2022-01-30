open Base
open Stdio
module V = Value

let process_line line genv =
  if List.mem [ "exit"; "quit" ] line ~equal:String.equal then Caml.exit 0;
  if String.is_empty line then genv
  else
    try
      match Lexer.tokenize line with
      | Result.Error err -> raise (Error.to_exn err)
      | Result.Ok toks -> (
          let expr = Parser.parse toks in
          let value = Evaluator.eval expr [] genv in
          let _ = printf "%s\n" (V.to_string value) in
          match value with V.Glet pair -> pair :: genv | _ -> genv)
    with exn ->
      printf "Invalid Expr (%s).\n" (Exn.to_string exn);
      genv

let rec repl genv =
  printf "> ";
  let _ = Out_channel.(flush stdout) in
  let line = In_channel.(input_line stdin) in
  match line with
  | None -> ()
  | Some line ->
      let genv =
        String.split line ~on:';' |> List.fold ~init:genv ~f:(fun acc line -> process_line line acc)
      in
      repl genv

let print_msg () =
  printf "Welcome to bunny REPL.\n";
  printf "To exit, type \"exit\" or \"quit\".\n"

let run () =
  let _ = print_msg () in
  let genv = Environment.empty in
  repl genv