open Base

type operation = Lowest | Compare | Sum | Product | Prefix | Group | Highest

let priority_of_operation = function
  | Lowest -> 0
  | Compare -> 1
  | Sum -> 2
  | Product -> 3
  | Prefix -> 4
  | Group -> 5
  | Highest -> 6

let priority_of_token token =
  let infix_op_of_token token =
    let open Token in
    match token with
    | Less | Greater | Equal | NotEqual -> Compare
    | Plus | Minus -> Sum
    | Asterisk -> Product
    | _ -> Lowest
  in
  let op = infix_op_of_token token in
  priority_of_operation op

let rec parse_prefix_int tokens idx =
  match List.nth_exn tokens idx with
  | Token.Integer x -> (Expression.IntLiteral x, idx)
  | _ -> failwith "Expected: Integer"

and parse_prefix_ident tokens idx =
  match List.nth_exn tokens idx with
  | Token.Ident s -> (
      let ident_expr = Expression.IdentExpr s in
      match List.nth tokens (idx + 1) with
      | Some Token.Lparen ->
          let args, args_idx = parse_args tokens (idx + 2) in
          let _ = verify_token tokens args_idx Token.Rparen "<fun-arguments> ends with" in
          (Expression.CallExpr (ident_expr, args), args_idx)
      | _ -> (ident_expr, idx))
  | _ -> failwith "Expected: Ident"

and parse_prefix_true tokens idx =
  match List.nth_exn tokens idx with
  | Token.True -> (Expression.BoolLiteral true, idx)
  | _ -> failwith "Expected: true"

and parse_prefix_false tokens idx =
  match List.nth_exn tokens idx with
  | Token.False -> (Expression.BoolLiteral false, idx)
  | _ -> failwith "Expected: false"

and parse_prefix_plus tokens idx =
  match List.nth_exn tokens idx with
  | Token.Plus ->
      parse_expr tokens (idx + 1) (priority_of_operation Lowest) (* ingore prefix plus *)
  | _ -> failwith "Expected: Plus"

and parse_prefix_minus tokens idx =
  match List.nth_exn tokens idx with
  | Token.Minus ->
      let expr, last_idx = parse_expr tokens (idx + 1) (priority_of_operation Prefix) in
      (Expression.PrefixExpr (Token.Minus, expr), last_idx)
  | _ -> failwith "Expected: Minus"

and parse_prefix_not tokens idx =
  match List.nth_exn tokens idx with
  | Token.Not ->
      let expr, last_idx = parse_expr tokens (idx + 1) (priority_of_operation Prefix) in
      (Expression.PrefixExpr (Token.Not, expr), last_idx)
  | _ -> failwith "Expected: Not"

and parse_prefix_lparen tokens idx =
  match List.nth_exn tokens idx with
  | Token.Lparen ->
      let expr, last_idx = parse_expr tokens (idx + 1) (priority_of_operation Lowest) in
      (expr, last_idx + 1)
  | _ -> failwith "Expected: Lparen"

and verify_token tokens idx expected_tok msg =
  let tok = List.nth_exn tokens idx in
  let ok =
    match (tok, expected_tok) with
    | Token.Ident _, Token.Ident _ -> true (* Does not check contents of ident. *)
    | _ -> Token.equal tok expected_tok
  in
  if not ok then
    failwith
      (Printf.sprintf "%s: '%s' <> '%s'" msg (Token.to_string tok) (Token.to_string expected_tok))

and parse_prefix_if tokens idx =
  (* if (condition) { consequence } else { alternative } *)
  let _ = verify_token tokens idx Token.If "<if> begins with" in
  let _ = verify_token tokens (idx + 1) Token.Lparen "<if-condition> begins with" in

  (* condition *)
  let cond_expr, cond_idx = parse_expr tokens (idx + 2) (priority_of_operation Lowest) in
  let _ = verify_token tokens (cond_idx + 1) Token.Rparen "<if-condition> ends with" in
  let _ = verify_token tokens (cond_idx + 2) Token.Lbrace "<if-consequence> begins with" in

  (* consequence *)
  let conq_expr, conq_idx = parse_expr tokens (cond_idx + 3) (priority_of_operation Lowest) in
  let _ = verify_token tokens (conq_idx + 1) Token.Rbrace "<if-consequence> ends with" in
  let _ = verify_token tokens (conq_idx + 2) Token.Else "<if> has else" in
  let _ = verify_token tokens (conq_idx + 3) Token.Lbrace "<if-alternative> begins with" in

  (* alternative *)
  let alt_expr, alt_idx = parse_expr tokens (conq_idx + 4) (priority_of_operation Lowest) in
  let _ = verify_token tokens (alt_idx + 1) Token.Rbrace "<if-alternative> ends with" in

  (Expression.IfExpr (cond_expr, conq_expr, alt_expr), alt_idx + 1)

and parse_args tokens idx =
  let rec sub cur_idx acc =
    let tok = List.nth_exn tokens cur_idx in
    match tok with
    | Token.Comma -> sub (cur_idx + 1) acc
    | Token.Rparen -> (acc, cur_idx)
    | _ ->
        let arg, arg_idx = parse_expr tokens cur_idx (priority_of_operation Lowest) in
        sub (arg_idx + 1) (arg :: acc)
  in
  let args, last_idx = sub idx [] in
  (List.rev args, last_idx)

and parse_prefix_fun tokens idx =
  (* fun (parameters) { body } *)
  let _ = verify_token tokens idx Token.Fun "<fun> begins with" in
  let _ = verify_token tokens (idx + 1) Token.Lparen "<fun-parameters> begins with" in

  (* parameters *)
  let parse_params tokens idx =
    let rec sub cur_idx acc =
      let tok = List.nth_exn tokens cur_idx in
      match tok with
      | Token.Comma -> sub (cur_idx + 1) acc
      | Token.Ident (_ as s) -> sub (cur_idx + 1) (s :: acc)
      | _ -> (acc, cur_idx)
    in
    let params, last_idx = sub idx [] in
    (List.rev params, last_idx)
  in
  let params, params_idx = parse_params tokens (idx + 2) in
  let _ = verify_token tokens params_idx Token.Rparen "<fun-parameters> ends with" in
  let _ = verify_token tokens (params_idx + 1) Token.Lbrace "<fun-body> begins with" in

  (* body *)
  let body_expr, body_idx = parse_expr tokens (params_idx + 2) (priority_of_operation Lowest) in
  let _ = verify_token tokens (body_idx + 1) Token.Rbrace "<fun-body> ends with" in

  (* call : fun (parameters) { body } (arguments) *)
  let fun_expr = Expression.FunExpr (params, body_expr) in
  match List.nth tokens (body_idx + 2) with
  | Some Token.Lparen ->
      let args, args_idx = parse_args tokens (body_idx + 3) in
      let _ = verify_token tokens args_idx Token.Rparen "<fun-arguments> ends with" in
      (Expression.CallExpr (fun_expr, args), args_idx + 1)
  | _ -> (fun_expr, body_idx + 1)

and parse_prefix_let tokens idx =
  (* let (ident) = (rhs) in (body) *)
  let _ = verify_token tokens idx Token.Let "<let> begins with" in
  let _ = verify_token tokens (idx + 1) (Token.Ident "") "<let-ident> begins with" in

  (* ident *)
  let expr, ident_idx = parse_expr tokens (idx + 1) (priority_of_operation Highest) in
  let _ = verify_token tokens (ident_idx + 1) Token.Equal "<let> has =" in
  match expr with
  | Expression.IdentExpr _ as ident_expr -> (
      (* rhs *)
      let rhs_expr, rhs_idx = parse_expr tokens (ident_idx + 2) (priority_of_operation Lowest) in
      match List.nth tokens (rhs_idx + 1) with
      | Some tok when Token.equal tok Token.In ->
          (* body *)
          let _ = verify_token tokens (rhs_idx + 1) Token.In "<let-body> begin with in" in
          let body_expr, body_idx =
            parse_expr tokens (rhs_idx + 2) (priority_of_operation Lowest)
          in
          (Expression.LetExpr (ident_expr, rhs_expr, Some body_expr), body_idx)
      | _ -> (Expression.LetExpr (ident_expr, rhs_expr, None), rhs_idx))
  | _ -> failwith "Expected: IdentExpr"

(* Get parse prefix function from Token. *)
and get_prefix_fn token =
  match token with
  | Token.Integer _ -> parse_prefix_int
  | Token.Plus -> parse_prefix_plus
  | Token.Minus -> parse_prefix_minus
  | Token.Not -> parse_prefix_not
  | Token.Lparen -> parse_prefix_lparen
  | Token.True -> parse_prefix_true
  | Token.False -> parse_prefix_false
  | Token.If -> parse_prefix_if
  | Token.Fun -> parse_prefix_fun
  | Token.Ident _ -> parse_prefix_ident
  | Token.Let -> parse_prefix_let
  | _ ->
      failwith (Printf.sprintf "Prefix function is not implemented for %s" (Token.to_string token))

and parse_infix_expr tokens idx left_expr =
  let token = List.nth_exn tokens idx in
  let pri = priority_of_token token in
  let right_expr, last_idx = parse_expr tokens (idx + 1) pri in
  (Expression.InfixExpr (token, left_expr, right_expr), last_idx)

and parse_expr tokens idx pri =
  if List.length tokens <= idx then failwith "Out of bound";

  let get_priority idx =
    if List.length tokens <= idx then priority_of_operation Lowest
    else priority_of_token (List.nth_exn tokens idx)
  in

  let parse_prefix () =
    let prefix_fn = get_prefix_fn (List.nth_exn tokens idx) in
    prefix_fn tokens idx
  in

  let rec loop idx left_expr =
    let next_pri = get_priority (idx + 1) in
    if pri < next_pri then
      let infix_fn = parse_infix_expr in
      let expr, last_idx = infix_fn tokens (idx + 1) left_expr in
      loop last_idx expr
    else (left_expr, idx)
  in

  let left_expr, last_idx = parse_prefix () in
  loop last_idx left_expr

let parse tokens =
  let expr, _ = parse_expr tokens 0 (priority_of_operation Lowest) in
  expr

let parse_string tokens = Expression.to_string (parse tokens)
