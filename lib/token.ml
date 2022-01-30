open Base

type t =
  | Int of int
  | Ident of string
  | Plus
  | Minus
  | Star
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Bang
  | Equal
  | Nequal
  | Less
  | Greater
  | Tilde
  | Comma
  | True
  | False
  | If
  | Else
  | Fun
  | Let
  | In
  | Eof
  | Illegal

let equal t1 t2 =
  match (t1, t2) with
  | Int x1, Int x2 -> Int.equal x1 x2
  | Ident x1, Ident x2 -> String.equal x1 x2
  | Plus, Plus
  | Minus, Minus
  | Star, Star
  | Lparen, Lparen
  | Rparen, Rparen
  | Lbrace, Lbrace
  | Rbrace, Rbrace
  | Bang, Bang
  | Equal, Equal
  | Nequal, Nequal
  | Less, Less
  | Greater, Greater
  | Tilde, Tilde
  | Comma, Comma
  | True, True
  | False, False
  | If, If
  | Else, Else
  | Fun, Fun
  | Let, Let
  | In, In
  | Eof, Eof
  | Illegal, Illegal ->
      true
  | _ -> false

let tokens =
  [
    (Plus, "+");
    (Minus, "-");
    (Star, "*");
    (Lparen, "(");
    (Rparen, ")");
    (Lbrace, "{");
    (Rbrace, "}");
    (Bang, "!");
    (Equal, "=");
    (Nequal, "<>");
    (Less, "<");
    (Greater, ">");
    (Tilde, "~");
    (Comma, ",");
    (True, "true");
    (False, "false");
    (If, "if");
    (Else, "else");
    (Fun, "fun");
    (Let, "let");
    (In, "in");
    (Eof, "EOF");
    (Illegal, "ILLEGAL");
  ]

let to_string tok =
  match tok with
  | Int x -> Int.to_string x
  | Ident x -> x
  | _ -> (
      match List.find tokens ~f:(fun (t, _) -> equal t tok) with
      | None -> "ILLEGAL"
      | Some (_, s) -> s)

let of_string str =
  match List.find tokens ~f:(fun (_, s) -> String.equal s str) with
  | Some (t, _) -> t
  | None -> Illegal

let of_char ch = of_string (String.of_char ch)
let is str = match of_string str with Illegal -> false | _ -> true
