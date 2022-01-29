open Base

let rec eval expr lenv genv =
  let open Expression in
  let open Value in
  match expr with
  | IntLiteral x -> Integer x
  | BoolLiteral b -> Boolean b
  | PrefixExpr (op, expr) -> (
      match op with
      | Token.Plus -> eval expr lenv genv
      | Token.Minus -> (
          match eval expr lenv genv with
          | Integer x -> Integer (-x)
          | Boolean _ | Function _ | AddGlobal _ -> failwith "type error")
      | Token.Not -> (
          match eval expr lenv genv with
          | Integer _ | Function _ | AddGlobal _ -> failwith (Printf.sprintf "type error")
          | Value.Boolean b -> Boolean (not b))
      | _ -> failwith (Printf.sprintf "Illegal prefix operator (%s)" (Token.to_string op)))
  | InfixExpr (op, expr1, expr2) as expr -> (
      let v1 = eval expr1 lenv genv in
      let v2 = eval expr2 lenv genv in
      match (op, v1, v2) with
      | Token.Plus, Integer x1, Integer x2 -> Integer (x1 + x2)
      | Token.Minus, Integer x1, Integer x2 -> Integer (x1 - x2)
      | Token.Asterisk, Integer x1, Integer x2 -> Integer (x1 * x2)
      | Token.Less, Integer x1, Integer x2 -> Boolean (x1 < x2)
      | Token.Greater, Integer x1, Integer x2 -> Boolean (x1 > x2)
      | Token.Equal, Integer x1, Integer x2 -> Boolean (x1 = x2)
      | Token.Equal, Boolean b1, Boolean b2 -> Boolean (Bool.equal b1 b2)
      | Token.NotEqual, Integer x1, Integer x2 -> Boolean (x1 <> x2)
      | Token.NotEqual, Boolean b1, Boolean b2 -> Boolean (not (Bool.equal b1 b2))
      | _ -> failwith (Printf.sprintf "Illegal infix expression (%s)" (Expression.to_string expr)))
  | IfExpr (cond, conq, alt) -> (
      match eval cond lenv genv with
      | Value.Boolean b -> if b then eval conq lenv genv else eval alt lenv genv
      | _ -> failwith "condition must be boolean")
  | IdentExpr s -> (
      match Environment.find lenv s with
      | Some expr -> eval expr lenv genv
      | None -> (
          match Environment.find genv s with
          | Some expr -> eval expr lenv genv
          | None -> failwith (Printf.sprintf "Undefined Identifier (%s)" s)))
  | FunExpr _ as expr -> Value.Function expr
  | CallExpr (expr, args) -> (
      match expr with
      | FunExpr (params, body) ->
          let new_env =
            List.map2_exn params args ~f:(fun p a -> (p, a))
            |> List.fold ~init:lenv ~f:(fun acc (p, a) -> Environment.add acc ~key:p ~value:a)
          in
          eval body new_env genv
      | IdentExpr _ -> (
          match eval expr lenv genv with
          | Function fun_expr -> eval (CallExpr (fun_expr, args)) lenv genv
          | _ -> failwith "type error")
      | _ -> failwith "type error")
  | LetExpr (ident, rhs, body) -> (
      match ident with
      | Expression.IdentExpr s -> (
          match body with
          | Some body -> eval body (Environment.add lenv ~key:s ~value:rhs) genv
          | None -> Value.AddGlobal (Environment.add genv ~key:s ~value:rhs))
      | _ -> failwith "type error")

let eval_string expr = Value.to_string (eval expr Environment.empty Environment.empty)
