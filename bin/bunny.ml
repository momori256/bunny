open Stdio
open Lib
open Base

type ('a, 'b) test_case = { input : 'b; expected : 'a; f : 'b -> 'a; equal : 'a -> 'a -> bool }

let test cases =
  cases |> List.map ~f:(fun { input; expected; f; equal } -> equal expected (f input))

let test_lexer =
  let make input expected =
    { input; expected; f = Lexer.tokenize; equal = List.equal Token.equal }
  in

  let single_tok = make "+-++-" Token.[ Plus; Minus; Plus; Plus; Minus ] in
  let single_int = make "12" Token.[ Integer 12 ] in
  let mix_tok_int = make "314+15" Token.[ Integer 314; Plus; Integer 15 ] in
  let whitespace_paren =
    make "  9 + (26 - 53)  "
      Token.[ Integer 9; Plus; Lparen; Integer 26; Minus; Integer 53; Rparen ]
  in

  let res =
    test [ single_tok; single_int; mix_tok_int; whitespace_paren ]
    |> List.fold ~init:true ~f:(fun acc res ->
           printf "%b\n" res;
           acc && res)
  in
  printf "test_lexer: %s\n" (if res then "OK" else "Failed")

let text_parser =
  let make input expected = { input; expected; f = Parser.parse; equal = String.equal } in

  let int_literal = make (Lexer.tokenize "12") "12" in
  let prefix_minus = make (Lexer.tokenize "-123") "(-123)" in
  let prefix_plus = make (Lexer.tokenize "+123") "123" in

  let infix_plus_1 = make (Lexer.tokenize "19 + 21") "(19 + 21)" in
  let infix_plus_2 = make (Lexer.tokenize "411 + 9 + 37") "((411 + 9) + 37)" in
  let infix_with_prefix = make (Lexer.tokenize "3 + 4 + -5") "((3 + 4) + (-5))" in
  let infix_product_1 = make (Lexer.tokenize "3 + 5 * 7") "(3 + (5 * 7))" in

  let group_1 = make (Lexer.tokenize "(3 + 5) + 7") "((3 + 5) + 7)" in
  let group_2 = make (Lexer.tokenize "(3 + 5) * -(7 + 9)") "((3 + 5) * (-(7 + 9)))" in

  let res =
    test
      [
        int_literal;
        prefix_minus;
        prefix_plus;
        infix_plus_1;
        infix_plus_2;
        infix_with_prefix;
        infix_product_1;
        group_1;
        group_2;
      ]
    |> List.fold ~init:true ~f:(fun acc res ->
           printf "%b\n" res;
           acc && res)
  in
  printf "test_parser: %s\n" (if res then "OK" else "Failed")
