open Base

let rec eval expr env =
  let open Expression in
  let open Value in
  match expr with
  | IntLiteral x -> Integer x
  | BoolLiteral b -> Boolean b
  | PrefixExpr (op, expr) -> (
      match op with
      | Token.Plus -> eval expr env
      | Token.Minus -> (
          match eval expr env with
          | Integer x -> Integer (-x)
          | Boolean _ | Function _ -> failwith "type error")
      | Token.Not -> (
          match eval expr env with
          | Integer _ | Function _ -> failwith (Printf.sprintf "type error")
          | Value.Boolean b -> Boolean (not b))
      | _ -> failwith (Printf.sprintf "Illegal prefix operator (%s)" (Token.to_string op)))
  | InfixExpr (op, expr1, expr2) as expr -> (
      let v1 = eval expr1 env in
      let v2 = eval expr2 env in
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
      match eval cond env with
      | Value.Boolean b -> if b then eval conq env else eval alt env
      | _ -> failwith "condition must be boolean")
  | IdentExpr s -> eval (List.Assoc.find_exn env s ~equal:String.equal) env
  | FunExpr _ as expr -> Value.Function expr
  | CallExpr (expr, args) -> (
      match expr with
      | FunExpr (params, body) ->
          let new_env =
            List.map2_exn params args ~f:(fun p a -> (p, a))
            |> List.fold ~init:env ~f:(fun acc (p, a) -> List.Assoc.add acc p a ~equal:String.equal)
          in
          eval body new_env
      | IdentExpr _ -> (
          match eval expr env with
          | Function fun_expr -> eval (CallExpr (fun_expr, args)) env
          | _ -> failwith "type error")
      | _ -> failwith "type error")

let eval_string expr = Value.to_string (eval expr [])
