open Lib
open Base

(* Test example. *)
let run_example_test () =
  let f1 x = 2 * x in
  let f1_test =
    Test.make ~name:"f1 test" ~equal:Int.equal ~f:f1 [ ("positive", 10, 5); ("negative", -20, -10) ]
  in

  let f2 x1 x2 = Int.to_float (f1 (x1 + x2)) +. 1.0 in
  let f2_test =
    let open Test in
    make ~name:"f2 test" ~equal:Float.equal ~f:(Uncurry.arg2 f2)
      [ ("case1", 7.0, (1, 2)); ("case2 (failure example)", -2.0, (1, 2)) ]
  in

  let _ = Test.run_and_print f1_test ~to_string:Int.to_string in
  Test.run_and_print f2_test ~to_string:Float.to_string

(* Lexer test. *)
let run_lexer_test () =
  let open Token in
  let lexer_test =
    Test.make ~name:"lexer test"
      ~equal:(fun r1 r2 ->
        Result.equal (fun ok1 ok2 -> List.equal Token.equal ok1 ok2) (fun _ _ -> false) r1 r2)
      ~f:Lexer.tokenize
      [
        ( "single",
          Ok [ Plus; Minus; Star; Lparen; Rparen; Lbrace; Rbrace; Bang; Equal; Less; Tilde ],
          "+-*(){}!=<~" );
        ("double", Ok [ Nequal; Comma; Greater; Less ], "<>,><");
        ("special", Ok [ True; False; If; Else; Fun; Let; In ], "true false if else fun let in");
        ("int", Ok [ Int 10; Minus; Int 91 ], "10 -91");
        ("identifier", Ok [ Ident "x"; Nequal; Ident "abc"; Equal; Ident "y1" ], "x<>abc=y1");
      ]
  in
  Test.run_and_print lexer_test ~to_string:(fun toks ->
      match toks with
      | Ok toks -> String.concat (List.map toks ~f:to_string) ~sep:", "
      | Error err -> Error.to_string_hum err)

(* Parser test. *)
let run_parser_test () =
  let parser_test =
    Test.make ~name:"parser test" ~equal:String.equal
      ~f:(fun s -> Parser.parse_string (Lexer.tokenize_exn s))
      [
        ("int", "12", "12");
        ("infix plus", "(55 + 21)", "55 + 21");
        ("infix minus", "(55 - 21)", "55 - 21");
        ("infix times", "((55 * 21) * 33)", "55 * 21 * 33");
        ("prefix minus", "(10 * (-10))", "10 * -10");
        ("prefix not", "(((~true) <> 5) = 5)", "~true<>5=5");
        ("suffix bang", "(10!)", "10!");
        ("suffix bang 2", "(5 + ((6!) * (-7)))", "5 + 6! * -7");
        ("identifier", "((abc * x) + y1)", "abc * x + y1");
        ("if", "(if ((3 <> 5)) then (1) else (2))", "if (3 <> 5) {1} else {2}");
        ( "if nested",
          "(if (true) then ((if (false) then (1) else (2))) else (3))",
          "if (true) { if (false) {1} else {2}} else {3}" );
        ("fun", "(fun (x, y) { (x + y) })", "fun (x, y) { x + y }");
        ("fun nested", "(fun (x) { (fun () { (x + 2) }) })", "fun (x) { fun () { x + 2 } }");
        ("call", "((fun (x, y) { (x + y) }) (10, 20))", "fun (x, y) { x + y } (10, 20)");
        ("let", "([x = 1] in (x + 2))", "let x = 1 in x + 2");
        ( "let 2",
          "([x = 2] in ([f = (fun (x) { (x * 2) })] in (f (x))))",
          "let x = 2 in let f = fun (x) { x * 2 } in f(x)" );
        ( "let nested",
          "([x = 1] in ([y = 2] in ([z = 3] in ((x + y) + z))))",
          "let x = 1 in let y = 2 in let z = 3 in x + y + z" );
        ("let global", "([x = 1])", "let x = 1");
      ]
  in
  Test.run_and_print parser_test ~to_string:String.to_string

(* Evaluator test. *)
let run_eval_test () =
  let eval_test =
    Test.make ~name:"eval test" ~equal:Value.equal
      ~f:(fun s -> Evaluator.eval (Parser.parse (Lexer.tokenize_exn s)) [] [])
      Value.
        [
          ("int", Int 10, "10");
          ("bool true", Bool true, "true");
          ("bool false", Bool false, "false");
          ("calc", Int ((10 * (2 + 4)) - 10), "10 * (2 + 4) - 10");
          ("calc 2", Int 840, "5! + (2 + 4)!");
          ( "if",
            Bool (if 3 + 3 < 4 * 2 then not true else not false),
            "if (3 + 3 < 4 * 2) { ~true } else { ~false }" );
          ( "if nested",
            Int (if 3 < 4 then if true then 1 else 2 else if false then 3 else 4),
            "if (3 < 4) { if (true) {1} else {2} } else { if (false) {3} else {4} }" );
          ( "fun",
            Fun
              (Parser.parse (Lexer.tokenize_exn "fun (x, abc) { if (x) {abc + 1} else {abc - 1}}")),
            "fun (x, abc) { if (x) {abc + 1} else {abc - 1}}" );
          ("let", Int 10, "let x = 2 in x + 8");
          ( "let nested",
            Int
              (let x1 = 3 in
               let x2 = 1 in
               let x3 = 4 in
               x1 + x2 + x3),
            "let x1 = 3 in let x2 = 1 in let x3 = 4 in x1 + x2 + x3" );
          ("call 1", Int ((fun x -> x * x * x) (-10 + 2)), "fun (x) { x * x * x } (-10 + 2)");
          ( "call 2",
            Bool
              (let f x = x * 2 < 10 in
               f (10 + 2)),
            "let f = fun (x) { x * 2 < 10 } in f (10 + 2)" );
        ]
  in
  Test.run_and_print eval_test ~to_string:Value.to_string

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  let pairs =
    [
      ("lexer", run_lexer_test);
      ("parser", run_parser_test);
      ("eval", run_eval_test);
      ("example", run_example_test);
    ]
  in
  if List.length args <= 1 then
    let names = String.concat (List.map pairs ~f:(fun (name, _) -> name)) ~sep:"|" in
    Stdio.printf "(usage) dune exec -- test/main.exe ([%s]+)\n" names
  else
    (* Register tests. *)
    let tests = Hashtbl.create (module String) in
    let _ = List.map pairs ~f:(fun (name, f) -> Hashtbl.add tests ~key:name ~data:f) in

    (* Run tests. *)
    List.dedup_and_sort args ~compare:(fun name1 name2 -> String.compare name1 name2)
    |> List.fold ~init:() ~f:(fun _ test ->
           match Hashtbl.find tests test with None -> () | Some f -> f ())
