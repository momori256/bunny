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

let to_string = function
  | Int x -> Int.to_string x
  | Ident s -> s
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Bang -> "!"
  | Equal -> "="
  | Nequal -> "<>"
  | Less -> "<"
  | Greater -> ">"
  | Tilde -> "~"
  | Comma -> ","
  | True -> "true"
  | False -> "false"
  | If -> "if"
  | Else -> "else"
  | Fun -> "fun"
  | Let -> "let"
  | In -> "in"
  | Eof -> "EOF"
  | Illegal -> "ILLEGAL"

let equal t1 t2 = String.equal (to_string t1) (to_string t2)
