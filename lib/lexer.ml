open Base

(* Whether a character is a number or not. *)
let is_number ch = Char.('0' <= ch && ch <= '9')

(* Whether a character is a letter or not. *)
let is_letter ch = Char.(('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_')

(* Start tokenizing from str.[idx], and return (Token.t, the last index of the token). *)
let rec step str idx acc =
  if String.length str <= idx then (Some Token.Eof, idx)
  else
    let single token = (Some token, idx) in
    let double token = (Some token, idx + 1) in

    let make_integer s =
      let num = Int.of_string s in
      Token.Integer num
    in

    let make_str_token = function
      | "true" -> Token.True
      | "false" -> Token.False
      | "if" -> Token.If
      | "else" -> Token.Else
      | "fun" -> Token.Fun
      | "let" -> Token.Let
      | "in" -> Token.In
      | _ as s -> Token.Ident s
    in

    let ch = str.[idx] in
    if String.length str - 1 = idx then
      (* Last index of str. *)
      if is_number ch then single (make_integer (acc ^ Char.escaped ch))
      else if is_letter ch then single (make_str_token (acc ^ Char.escaped ch))
      else
        match ch with
        | '+' -> single Token.Plus
        | '-' -> single Token.Minus
        | '*' -> single Token.Asterisk
        | '(' -> single Token.Lparen
        | ')' -> single Token.Rparen
        | '{' -> single Token.Lbrace
        | '}' -> single Token.Rbrace
        | '!' -> single Token.Bang
        | '=' -> single Token.Equal
        | '~' -> single Token.Not
        | ',' -> single Token.Comma
        | '>' -> single Token.Greater
        | '<' -> single Token.Less
        | ' ' -> (None, idx)
        | _ -> failwith ""
    else
      (* Next character exists. *)
      let next = str.[idx + 1] in
      if is_number ch then
        let next_acc = acc ^ Char.escaped ch in
        if is_number next then step str (idx + 1) next_acc else single (make_integer next_acc)
      else if is_letter ch then
        let next_acc = acc ^ Char.escaped ch in
        if is_letter next then step str (idx + 1) next_acc else single (make_str_token next_acc)
      else
        match (ch, next) with
        | '+', _ -> single Token.Plus
        | '-', _ -> single Token.Minus
        | '*', _ -> single Token.Asterisk
        | '(', _ -> single Token.Lparen
        | ')', _ -> single Token.Rparen
        | '{', _ -> single Token.Lbrace
        | '}', _ -> single Token.Rbrace
        | '!', _ -> single Token.Bang
        | '=', _ -> single Token.Equal
        | '~', _ -> single Token.Not
        | ',', _ -> single Token.Comma
        | '>', _ -> single Token.Greater
        | '<', '>' -> double Token.NotEqual
        | '<', _ -> single Token.Less
        | ' ', _ -> (None, idx)
        | (_ as c1), (_ as c2) -> failwith (Printf.sprintf "Unknown char '%c', '%c'" c1 c2)

let tokenize str =
  let rec sub idx =
    let tok, last_idx = step str idx "" in
    match tok with
    | None -> sub (last_idx + 1) (* Ignore whitespace. *)
    | Some Token.Eof -> []
    | Some tok -> tok :: sub (last_idx + 1)
  in
  sub 0
