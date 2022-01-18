open Stdio
open Lib

let rec repl () =
  printf "> ";
  let _ = flush Out_channel.stdout in
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> ()
  | Some line -> (
      if List.mem line [ "exit"; "quit" ] then ()
      else
        try
          let tokens = Lexer.tokenize line in
          let expr = Parser.parse tokens in
          let result = Evaluator.eval expr in
          let _ = printf "%s\n" (Value.to_string result) in
          repl ()
        with _ -> printf "Invalid expression.\n")

let print_msg () =
  printf "Welcome to bunny REPL.\n";
  printf "To exit, type \"exit\" or \"quit\".\n"

let () =
  let _ = print_msg () in
  repl ()
