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

  let single_tok =
    make "+-*()!=>~" Token.[ Plus; Minus; Asterisk; Lparen; Rparen; Bang; Equal; Greater; Not ]
  in
  let single_int = make "12" Token.[ Integer 12 ] in
  let single_true_false = make "true false" Token.[ True; False ] in

  let double_tok_1 = make "314<>15" Token.[ Integer 314; NotEqual; Integer 15 ] in
  let double_tok_2 = make "!<><" Token.[ Bang; NotEqual; Less ] in

  let whitespace_included_1 =
    make "  ~ ( 99 <>  100  ) <> ( 4  > 1)  "
      [
        Not;
        Lparen;
        Integer 99;
        NotEqual;
        Integer 100;
        Rparen;
        NotEqual;
        Lparen;
        Integer 4;
        Greater;
        Integer 1;
        Rparen;
      ]
  in
  let whitespace_included_2 =
    make " (5 = 5) <> ~false"
      Token.[ Lparen; Integer 5; Equal; Integer 5; Rparen; NotEqual; Not; False ]
  in

  let if_1 =
    make "if (true) { 4 + 3 } else { 2 * 7 }"
      Token.
        [
          If;
          Lparen;
          True;
          Rparen;
          Lbrace;
          Integer 4;
          Plus;
          Integer 3;
          Rbrace;
          Else;
          Lbrace;
          Integer 2;
          Asterisk;
          Integer 7;
          Rbrace;
        ]
  in

  let res =
    test
      [
        single_tok;
        single_int;
        single_true_false;
        double_tok_1;
        double_tok_2;
        whitespace_included_1;
        whitespace_included_2;
        if_1;
      ]
    |> List.fold ~init:true ~f:(fun acc res ->
           printf "%b\n" res;
           acc && res)
  in
  printf "test_lexer: %s\n" (if res then "OK" else "Failed")

let text_parser =
  let make input expected = { input; expected; f = Parser.parse_string; equal = String.equal } in

  let int_literal = make (Lexer.tokenize "12") "12" in
  let prefix_minus = make (Lexer.tokenize "-123") "(-123)" in
  let prefix_plus = make (Lexer.tokenize "+123") "123" in
  let prefix_not = make (Lexer.tokenize "~true") "(~true)" in

  let infix_plus_1 = make (Lexer.tokenize "19 + 21") "(19 + 21)" in
  let infix_plus_2 = make (Lexer.tokenize "411 + 9 + 37") "((411 + 9) + 37)" in
  let infix_with_prefix = make (Lexer.tokenize "3 + 4 + -5") "((3 + 4) + (-5))" in
  let infix_product_1 = make (Lexer.tokenize "3 + 5 * 7") "(3 + (5 * 7))" in
  let infix_less = make (Lexer.tokenize "5 < 3") "(5 < 3)" in
  let infix_greater_1 = make (Lexer.tokenize "5 > 3") "(5 > 3)" in
  let infix_greater_2 = make (Lexer.tokenize "1 + 2 > 3 + 4") "((1 + 2) > (3 + 4))" in
  let infix_equal = make (Lexer.tokenize "5 = 3") "(5 = 3)" in
  let infix_notequal = make (Lexer.tokenize "~true <> ~false") "((~true) <> (~false))" in

  let group_1 = make (Lexer.tokenize "(3 + 5) + 7") "((3 + 5) + 7)" in
  let group_2 = make (Lexer.tokenize "(3 + 5) * -(7 + 9)") "((3 + 5) * (-(7 + 9)))" in
  let group_3 = make (Lexer.tokenize "5 + 3 <> 2 * 4") "((5 + 3) <> (2 * 4))" in

  let if_1 = make (Lexer.tokenize "if (true) { 1 } else { 2 }") "(if (true) then (1) else (2))" in
  let if_2 =
    make
      (Lexer.tokenize "if (true) { 4 + 3 } else { 2 * 7 }")
      "(if (true) then ((4 + 3)) else ((2 * 7)))"
  in
  let if_3 =
    make
      (Lexer.tokenize "if (true) { if (false) { 1 } else { 2 } } else { 3 }")
      "(if (true) then ((if (false) then (1) else (2))) else (3))"
  in
  let res =
    test
      [
        int_literal;
        prefix_minus;
        prefix_plus;
        prefix_not;
        infix_plus_1;
        infix_plus_2;
        infix_with_prefix;
        infix_product_1;
        infix_less;
        infix_greater_1;
        infix_greater_2;
        infix_equal;
        infix_notequal;
        group_1;
        group_2;
        group_3;
        if_1;
        if_2;
        if_3;
      ]
    |> List.fold ~init:true ~f:(fun acc res ->
           printf "%b\n" res;
           acc && res)
  in
  printf "test_parser: %s\n" (if res then "OK" else "Failed")

let text_evaluator =
  let make input expected = { input; expected; f = Evaluator.eval_string; equal = String.equal } in

  let int_literal = make (Parser.parse (Lexer.tokenize "12")) (Int.to_string 12) in
  let bool_literal = make (Parser.parse (Lexer.tokenize "true")) (Bool.to_string true) in
  let prefix_minus = make (Parser.parse (Lexer.tokenize "123 + 54")) (Int.to_string (123 + 54)) in
  let prefix_plus = make (Parser.parse (Lexer.tokenize "-123 * 99")) (Int.to_string (-123 * 99)) in
  let prefix_not =
    make
      (Parser.parse (Lexer.tokenize "~true <> (3 = 5)"))
      (Bool.to_string (Bool.( <> ) (not true) (3 = 5)))
  in
  let compare_1 = make (Parser.parse (Lexer.tokenize "12 < 56")) (Bool.to_string (12 < 56)) in
  let compare_2 =
    make (Parser.parse (Lexer.tokenize "1 + 2 > 3 + 4")) (Bool.to_string (1 + 2 > 3 + 4))
  in
  let group_1 =
    make (Parser.parse (Lexer.tokenize "(3 + 5) * -(7 + 9)")) (Int.to_string ((3 + 5) * -(7 + 9)))
  in

  let if_1 =
    make
      (Parser.parse (Lexer.tokenize "if (true) { 1 } else { 2 }"))
      (Int.to_string (if true then 1 else 2))
  in
  let if_2 =
    make
      (Parser.parse (Lexer.tokenize "if (2 > 3) { 1 } else { if (1 = 1) { 2 } else { 3 } }"))
      (Int.to_string (if 2 > 3 then 1 else if 1 = 1 then 2 else 3))
  in

  let res =
    test
      [
        int_literal;
        bool_literal;
        prefix_minus;
        prefix_plus;
        prefix_not;
        compare_1;
        compare_2;
        group_1;
        if_1;
        if_2;
      ]
    |> List.fold ~init:true ~f:(fun acc res ->
           printf "%b\n" res;
           acc && res)
  in
  printf "test_evaluator: %s\n" (if res then "OK" else "Failed")
