type t = Integer of int | Plus | Minus | Lparen | Rparen | Illegal

let equal t1 t2 =
  match (t1, t2) with
  | Plus, Plus | Minus, Minus | Lparen, Lparen | Rparen, Rparen -> true
  | Integer x1, Integer x2 -> x1 = x2
  | _, _ -> false

let of_char = function
  | '+' -> Plus
  | '-' -> Minus
  | '(' -> Lparen
  | ')' -> Rparen
  | _ -> Illegal
