open Base

let rec eval expr =
  let open Expression in
  match expr with
  | IntLiteral x -> x
  | PrefixExpr (op, expr) -> (
      match op with
      | Token.Plus -> eval expr
      | Token.Minus -> -eval expr
      | _ -> failwith (Printf.sprintf "Illegal prefix operator (%s)" (Token.to_string op)))
  | InfixExpr (op, expr1, expr2) -> (
      match op with
      | Token.Plus -> eval expr1 + eval expr2
      | Token.Minus -> eval expr1 - eval expr2
      | Token.Asterisk -> eval expr1 * eval expr2
      | _ -> failwith (Printf.sprintf "Illegal infix operator (%s)" (Token.to_string op)))
