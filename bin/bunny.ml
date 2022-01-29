open Stdio
open Lib
open Base

let process_line line genv =
  if List.mem [ "exit"; "quit" ] line ~equal:String.equal then Caml.exit 0;
  if String.is_empty line then genv
  else
    try
      let tokens = Lexer.tokenize line in
      let expr = Parser.parse tokens in
      let value = Evaluator.eval expr [] genv in
      let _ = printf "%s\n" (Value.to_string value) in
      match value with
      | Value.Integer _ | Value.Boolean _ | Value.Function _ -> genv
      | Value.AddGlobal pair -> pair :: genv
    with _ ->
      printf "Invalid expression.\n";
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

let () =
  let _ = print_msg () in
  let genv = Environment.empty in
  repl genv