open Base

type operator = Token.t

type t =
  | IntLiteral of int
  | BoolLiteral of bool
  | PrefixExpr of operator * t
  | InfixExpr of operator * t * t
  | IfExpr of t * t * t
(* | SuffixExpr of operator * expression *)

let rec equal t1 t2 =
  match (t1, t2) with
  | IntLiteral x1, IntLiteral x2 -> x1 = x2
  | BoolLiteral b1, BoolLiteral b2 -> Bool.equal b1 b2
  | PrefixExpr (op1, expr1), PrefixExpr (op2, expr2) -> Token.equal op1 op2 && equal expr1 expr2
  | InfixExpr (op1, l1, r1), InfixExpr (op2, l2, r2) ->
      Token.equal op1 op2 && equal l1 l2 && equal r1 r2
  | _, _ -> false

let rec to_string = function
  | IntLiteral x -> Int.to_string x
  | BoolLiteral b -> Bool.to_string b
  | PrefixExpr (op, expr) -> Printf.sprintf "(%s%s)" (Token.to_string op) (to_string expr)
  | InfixExpr (op, expr1, expr2) ->
      let op = Token.to_string op in
      let expr1 = to_string expr1 in
      let expr2 = to_string expr2 in
      Printf.sprintf "(%s %s %s)" expr1 op expr2
  | IfExpr (cond, cons, alt) ->
      let cond = to_string cond in
      let conq = to_string cons in
      let alt = to_string alt in
      Printf.sprintf "(if (%s) then (%s) else (%s))" cond conq alt
