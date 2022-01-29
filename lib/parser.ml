open Base
module T = Token
module E = Expression

type operation = Lowest | Compare | Sum | Product | Prefix | Group | Highest

let priority_of_operation = function
  | Lowest -> 0
  | Compare -> 1
  | Sum -> 2
  | Product -> 3
  | Prefix -> 4
  | Group -> 5
  | Highest -> 6

let priority_of_tok tok =
  let infix_op_of_tok tok =
    let open Token in
    match tok with
    | Less | Greater | Equal | Nequal -> Compare
    | Plus | Minus -> Sum
    | Star -> Product
    | _ -> Lowest
  in
  let op = infix_op_of_tok tok in
  priority_of_operation op

let rec parse_prefix_int toks idx =
  match List.nth_exn toks idx with T.Int x -> (E.Int x, idx) | _ -> failwith "Expected: Integer"

and parse_prefix_ident toks idx =
  match List.nth_exn toks idx with
  | T.Ident s -> (
      let ident_expr = E.Ident s in
      match List.nth toks (idx + 1) with
      | Some T.Lparen ->
          let args, args_idx = parse_args toks (idx + 2) in
          let _ = verify_tok toks args_idx T.Rparen "<fun-arguments> ends with" in
          (E.Call (ident_expr, args), args_idx)
      | _ -> (ident_expr, idx))
  | _ -> failwith "Expected: Ident"

and parse_prefix_true toks idx =
  match List.nth_exn toks idx with T.True -> (E.Bool true, idx) | _ -> failwith "Expected: true"

and parse_prefix_false toks idx =
  match List.nth_exn toks idx with
  | T.False -> (E.Bool false, idx)
  | _ -> failwith "Expected: false"

and parse_prefix_plus toks idx =
  match List.nth_exn toks idx with
  | T.Plus -> parse_expr toks (idx + 1) (priority_of_operation Lowest) (* ingore prefix plus *)
  | _ -> failwith "Expected: Plus"

and parse_prefix_minus toks idx =
  match List.nth_exn toks idx with
  | T.Minus ->
      let expr, last_idx = parse_expr toks (idx + 1) (priority_of_operation Prefix) in
      (E.Prefix (T.Minus, expr), last_idx)
  | _ -> failwith "Expected: Minus"

and parse_prefix_not toks idx =
  match List.nth_exn toks idx with
  | T.Tilde ->
      let expr, last_idx = parse_expr toks (idx + 1) (priority_of_operation Prefix) in
      (E.Prefix (T.Tilde, expr), last_idx)
  | _ -> failwith "Expected: Not"

and parse_prefix_lparen toks idx =
  match List.nth_exn toks idx with
  | T.Lparen ->
      let expr, last_idx = parse_expr toks (idx + 1) (priority_of_operation Lowest) in
      (expr, last_idx + 1)
  | _ -> failwith "Expected: Lparen"

and verify_tok toks idx expected_tok msg =
  let tok = List.nth_exn toks idx in
  let ok =
    match (tok, expected_tok) with
    | T.Ident _, T.Ident _ -> true (* Does not check contents of ident. *)
    | _ -> T.equal tok expected_tok
  in
  if not ok then
    failwith (Printf.sprintf "%s: '%s' <> '%s'" msg (T.to_string tok) (T.to_string expected_tok))

and parse_prefix_if toks idx =
  (* if (condition) { consequence } else { alternative } *)
  let _ = verify_tok toks idx T.If "<if> begins with" in
  let _ = verify_tok toks (idx + 1) T.Lparen "<if-condition> begins with" in

  (* condition *)
  let cond_expr, cond_idx = parse_expr toks (idx + 2) (priority_of_operation Lowest) in
  let _ = verify_tok toks (cond_idx + 1) T.Rparen "<if-condition> ends with" in
  let _ = verify_tok toks (cond_idx + 2) T.Lbrace "<if-consequence> begins with" in

  (* consequence *)
  let conq_expr, conq_idx = parse_expr toks (cond_idx + 3) (priority_of_operation Lowest) in
  let _ = verify_tok toks (conq_idx + 1) T.Rbrace "<if-consequence> ends with" in
  let _ = verify_tok toks (conq_idx + 2) T.Else "<if> has else" in
  let _ = verify_tok toks (conq_idx + 3) T.Lbrace "<if-alternative> begins with" in

  (* alternative *)
  let alt_expr, alt_idx = parse_expr toks (conq_idx + 4) (priority_of_operation Lowest) in
  let _ = verify_tok toks (alt_idx + 1) T.Rbrace "<if-alternative> ends with" in

  (E.If (cond_expr, conq_expr, alt_expr), alt_idx + 1)

and parse_args toks idx =
  let rec sub cur_idx acc =
    let tok = List.nth_exn toks cur_idx in
    match tok with
    | T.Comma -> sub (cur_idx + 1) acc
    | T.Rparen -> (acc, cur_idx)
    | _ ->
        let arg, arg_idx = parse_expr toks cur_idx (priority_of_operation Lowest) in
        sub (arg_idx + 1) (arg :: acc)
  in
  let args, last_idx = sub idx [] in
  (List.rev args, last_idx)

and parse_prefix_fun toks idx =
  (* fun (parameters) { body } *)
  let _ = verify_tok toks idx T.Fun "<fun> begins with" in
  let _ = verify_tok toks (idx + 1) T.Lparen "<fun-parameters> begins with" in

  (* parameters *)
  let parse_params toks idx =
    let rec sub cur_idx acc =
      let tok = List.nth_exn toks cur_idx in
      match tok with
      | T.Comma -> sub (cur_idx + 1) acc
      | T.Ident (_ as s) -> sub (cur_idx + 1) (s :: acc)
      | _ -> (acc, cur_idx)
    in
    let params, last_idx = sub idx [] in
    (List.rev params, last_idx)
  in
  let params, params_idx = parse_params toks (idx + 2) in
  let _ = verify_tok toks params_idx T.Rparen "<fun-parameters> ends with" in
  let _ = verify_tok toks (params_idx + 1) T.Lbrace "<fun-body> begins with" in

  (* body *)
  let body_expr, body_idx = parse_expr toks (params_idx + 2) (priority_of_operation Lowest) in
  let _ = verify_tok toks (body_idx + 1) T.Rbrace "<fun-body> ends with" in

  (* call : fun (parameters) { body } (arguments) *)
  let fun_expr = E.Fun (params, body_expr) in
  match List.nth toks (body_idx + 2) with
  | Some T.Lparen ->
      let args, args_idx = parse_args toks (body_idx + 3) in
      let _ = verify_tok toks args_idx T.Rparen "<fun-arguments> ends with" in
      (E.Call (fun_expr, args), args_idx + 1)
  | _ -> (fun_expr, body_idx + 1)

and parse_prefix_let toks idx =
  (* let (ident) = (rhs) in (body) *)
  let _ = verify_tok toks idx T.Let "<let> begins with" in
  let _ = verify_tok toks (idx + 1) (T.Ident "") "<let-ident> begins with" in

  (* ident *)
  let expr, ident_idx = parse_expr toks (idx + 1) (priority_of_operation Highest) in
  let _ = verify_tok toks (ident_idx + 1) T.Equal "<let> has =" in
  match expr with
  | E.Ident _ as ident_expr -> (
      (* rhs *)
      let rhs_expr, rhs_idx = parse_expr toks (ident_idx + 2) (priority_of_operation Lowest) in
      match List.nth toks (rhs_idx + 1) with
      | Some tok when T.equal tok T.In ->
          (* body *)
          let _ = verify_tok toks (rhs_idx + 1) T.In "<let-body> begin with in" in
          let body_expr, body_idx = parse_expr toks (rhs_idx + 2) (priority_of_operation Lowest) in
          (E.Let (ident_expr, rhs_expr, Some body_expr), body_idx)
      | _ -> (E.Let (ident_expr, rhs_expr, None), rhs_idx))
  | _ -> failwith "Expected: Ident"

(* Get parse prefix function from T. *)
and get_prefix_fn tok =
  match tok with
  | T.Int _ -> parse_prefix_int
  | T.Plus -> parse_prefix_plus
  | T.Minus -> parse_prefix_minus
  | T.Tilde -> parse_prefix_not
  | T.Lparen -> parse_prefix_lparen
  | T.True -> parse_prefix_true
  | T.False -> parse_prefix_false
  | T.If -> parse_prefix_if
  | T.Fun -> parse_prefix_fun
  | T.Ident _ -> parse_prefix_ident
  | T.Let -> parse_prefix_let
  | _ -> failwith (Printf.sprintf "Prefix function is not implemented for %s" (T.to_string tok))

and parse_infix_expr toks idx left_expr =
  let tok = List.nth_exn toks idx in
  let pri = priority_of_tok tok in
  let right_expr, last_idx = parse_expr toks (idx + 1) pri in
  (E.Infix (tok, left_expr, right_expr), last_idx)

and parse_expr toks idx pri =
  if List.length toks <= idx then failwith "Out of bound";

  let get_priority idx =
    if List.length toks <= idx then priority_of_operation Lowest
    else priority_of_tok (List.nth_exn toks idx)
  in

  let parse_prefix () =
    let prefix_fn = get_prefix_fn (List.nth_exn toks idx) in
    prefix_fn toks idx
  in

  let rec loop idx left_expr =
    let next_pri = get_priority (idx + 1) in
    if pri < next_pri then
      let infix_fn = parse_infix_expr in
      let expr, last_idx = infix_fn toks (idx + 1) left_expr in
      loop last_idx expr
    else (left_expr, idx)
  in

  let left_expr, last_idx = parse_prefix () in
  loop last_idx left_expr

let parse toks =
  let expr, _ = parse_expr toks 0 (priority_of_operation Lowest) in
  expr

let parse_string toks = E.to_string (parse toks)
