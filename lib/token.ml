type t =
  | Integer of int
  | Ident of string
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
  | Comma
  | True
  | False
  | If
  | Else
  | Fun
  | Eof
  | Illegal

let to_string = function
  | Integer x -> Int.to_string x
  | Ident s -> s
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
  | Comma -> ","
  | True -> "true"
  | False -> "false"
  | If -> "if"
  | Else -> "else"
  | Fun -> "fun"
  | Eof -> "EOF"
  | Illegal -> "ILLEGAL"

let equal t1 t2 = String.equal (to_string t1) (to_string t2)
