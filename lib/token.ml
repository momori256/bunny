type t = Integer of int | Plus | Minus | Asterisk | Lparen | Rparen | Illegal

let equal t1 t2 =
  match (t1, t2) with
  | Plus, Plus
  | Minus, Minus
  | Asterisk, Asterisk
  | Lparen, Lparen
  | Rparen, Rparen ->
      true
  | Integer x1, Integer x2 -> x1 = x2
  | _, _ -> false

let of_char = function
  | '+' -> Plus
  | '-' -> Minus
  | '*' -> Asterisk
  | '(' -> Lparen
  | ')' -> Rparen
  | _ -> Illegal

let to_string = function
  | Integer x -> Printf.sprintf "%d" x
  | Plus -> "+"
  | Minus -> "-"
  | Asterisk -> "*"
  | Lparen -> "("
  | Rparen -> ")"
  | Illegal -> "x"
