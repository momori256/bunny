open Base
module T = Token

(* Start Tokizing from chs[idx], and return (Token.t, the last index of the token). *)
let rec step chs idx acc =
  let is_letter ch = Char.(('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_') in
  let is_number ch = Char.('0' <= ch && ch <= '9') in

  let single tok = Result.Ok (Some tok, idx) in
  let double tok = Result.Ok (Some tok, idx + 1) in
  let make_err str = Error (Error.of_string str) in

  let make_tok_acc acc =
    let make_int_tok s =
      try
        let num = Int.of_string s in
        Ok (T.Int num)
      with _ -> make_err (Printf.sprintf "Invalid number '%s'" s)
    in
    let make_str_tok = function
      | "true" -> Ok T.True
      | "false" -> Ok T.False
      | "if" -> Ok T.If
      | "else" -> Ok T.Else
      | "fun" -> Ok T.Fun
      | "let" -> Ok T.Let
      | "in" -> Ok T.In
      | _ as s -> Ok (T.Ident s)
    in
    let acc = List.rev acc in
    match List.hd acc with
    | Some ch when is_number ch -> make_int_tok (String.of_char_list acc)
    | Some ch when is_letter ch -> make_str_tok (String.of_char_list acc)
    | _ -> make_err "Internal error (accumulator is empty)."
  in

  match (List.nth chs idx, List.nth chs (idx + 1)) with
  | None, _ -> single T.Eof
  | Some ('+' as ch), _
  | Some ('-' as ch), _
  | Some ('*' as ch), _
  | Some ('(' as ch), _
  | Some (')' as ch), _
  | Some ('{' as ch), _
  | Some ('}' as ch), _
  | Some ('!' as ch), _
  | Some ('=' as ch), _
  | Some ('~' as ch), _
  | Some (',' as ch), _
  | Some ('>' as ch), _ ->
      single (T.of_char ch)
  | Some '<', Some '>' -> double T.Nequal
  | Some '<', _ -> single T.Less
  | Some ' ', _ -> Result.Ok (None, idx) (* Skip whitespace. *)
  (* Number or letter. *)
  | Some ch1, Some ch2 when is_number ch2 || is_letter ch2 ->
      step chs (idx + 1) (ch1 :: acc) (* Continue parsing. *)
  | Some ch1, Some ch2 when T.is (String.of_char ch2) || Char.equal ch2 ' ' -> (
      match make_tok_acc (ch1 :: acc) with
      | Error _ as err -> err
      | Ok tok -> single tok (* Complete parsing number or letter. *))
  | Some ch1, None -> (
      match make_tok_acc (ch1 :: acc) with
      | Error _ as err -> err
      | Ok tok -> single tok (* Complete parsing number or letter. *))
  | Some (_ as ch), _ -> make_err (Printf.sprintf "Invalid character '%c'" ch)

let tokenize str =
  let chs = String.to_list str in
  let rec sub idx acc =
    match step chs idx [] with
    | Result.Ok (None, idx) -> sub (idx + 1) acc
    | Result.Ok (Some T.Eof, _) -> Result.Ok (List.rev acc)
    | Result.Ok (Some tok, idx) -> sub (idx + 1) (tok :: acc)
    | Result.Error _ as err -> err
  in
  sub 0 []

let tokenize_exn str =
  match tokenize str with Result.Ok toks -> toks | Result.Error s -> raise (Error.to_exn s)
