open Base

let rec eval expr =
  let open Expression in
  let open Value in
  match expr with
  | IntLiteral x -> Integer x
  | BoolLiteral b -> Boolean b
  | PrefixExpr (op, expr) -> (
      match op with
      | Token.Plus -> eval expr
      | Token.Minus -> (
          match eval expr with Integer x -> Integer (-x) | Boolean _ -> failwith "type error")
      | Token.Not -> (
          match eval expr with
          | Integer x -> failwith (Printf.sprintf "type error: ~%d" x)
          | Value.Boolean b -> Boolean (not b))
      | _ -> failwith (Printf.sprintf "Illegal prefix operator (%s)" (Token.to_string op)))
  | InfixExpr (op, expr1, expr2) as expr -> (
      let v1 = eval expr1 in
      let v2 = eval expr2 in
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

let eval_string expr = Value.to_string (eval expr)
