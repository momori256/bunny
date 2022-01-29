open Base
module T = Token

(* Whether a character is a number or not. *)
let is_number ch = Char.('0' <= ch && ch <= '9')

(* Whether a character is a letter or not. *)
let is_letter ch = Char.(('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_')

(* Start Tokizing from str.[idx], and return (T.t, the last index of the Tok). *)
let rec step str idx acc =
  if String.length str <= idx then (Some T.Eof, idx)
  else
    let single tok = (Some tok, idx) in
    let double tok = (Some tok, idx + 1) in

    let make_integer s =
      let num = Int.of_string s in
      T.Int num
    in

    let make_str_Tok = function
      | "true" -> T.True
      | "false" -> T.False
      | "if" -> T.If
      | "else" -> T.Else
      | "fun" -> T.Fun
      | "let" -> T.Let
      | "in" -> T.In
      | _ as s -> T.Ident s
    in

    let ch = str.[idx] in
    if String.length str - 1 = idx then
      (* Last index of str. *)
      if is_number ch then single (make_integer (acc ^ Char.escaped ch))
      else if is_letter ch then single (make_str_Tok (acc ^ Char.escaped ch))
      else
        match ch with
        | '+' -> single T.Plus
        | '-' -> single T.Minus
        | '*' -> single T.Star
        | '(' -> single T.Lparen
        | ')' -> single T.Rparen
        | '{' -> single T.Lbrace
        | '}' -> single T.Rbrace
        | '!' -> single T.Bang
        | '=' -> single T.Equal
        | '~' -> single T.Tilde
        | ',' -> single T.Comma
        | '>' -> single T.Greater
        | '<' -> single T.Less
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
        if is_letter next then step str (idx + 1) next_acc else single (make_str_Tok next_acc)
      else
        match (ch, next) with
        | '+', _ -> single T.Plus
        | '-', _ -> single T.Minus
        | '*', _ -> single T.Star
        | '(', _ -> single T.Lparen
        | ')', _ -> single T.Rparen
        | '{', _ -> single T.Lbrace
        | '}', _ -> single T.Rbrace
        | '!', _ -> single T.Bang
        | '=', _ -> single T.Equal
        | '~', _ -> single T.Tilde
        | ',', _ -> single T.Comma
        | '>', _ -> single T.Greater
        | '<', '>' -> double T.Nequal
        | '<', _ -> single T.Less
        | ' ', _ -> (None, idx)
        | (_ as c1), (_ as c2) -> failwith (Printf.sprintf "Unknown char '%c', '%c'" c1 c2)

let tokenize str =
  let rec sub idx =
    let tok, last_idx = step str idx "" in
    match tok with
    | None -> sub (last_idx + 1) (* Ignore whitespace. *)
    | Some T.Eof -> []
    | Some tok -> tok :: sub (last_idx + 1)
  in
  sub 0
