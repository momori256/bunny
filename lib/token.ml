type t =
  | Integer of int
  | Plus
  | Minus
  | Asterisk
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Bang
  | Equal
  | NotEqual
  | Less
  | Greater
  | Not
  | True
  | False
  | If
  | Else
  | Eof
  | Illegal

let to_string = function
  | Integer x -> Int.to_string x
  | Plus -> "+"
  | Minus -> "-"
  | Asterisk -> "*"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Bang -> "!"
  | Equal -> "="
  | NotEqual -> "<>"
  | Less -> "<"
  | Greater -> ">"
  | Not -> "~"
  | True -> "true"
  | False -> "false"
  | If -> "if"
  | Else -> "else"
  | Eof -> "EOF"
  | Illegal -> "ILLEGAL"

let equal t1 t2 = String.equal (to_string t1) (to_string t2)
