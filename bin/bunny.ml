open Stdio
open Lib
open Base

type ('a, 'b) test_case = {
  input : 'b;
  expected : 'a;
  f : 'b -> 'a;
  equal : 'a -> 'a -> bool;
}

let test cases =
  cases
  |> List.map ~f:(fun { input; expected; f; equal } -> equal expected (f input))

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
