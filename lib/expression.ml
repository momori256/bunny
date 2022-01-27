open Base

type operator = Token.t

type t =
  | IntLiteral of int
  | BoolLiteral of bool
  | PrefixExpr of operator * t
  | InfixExpr of operator * t * t
  | IfExpr of t * t * t
  | IdentExpr of string
  | FunExpr of string list * t
  | CallExpr of t * t list
  | LetExpr of t * t * t
(* ident rhs body *)

(* (ident, t), t *)
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
  | IdentExpr s -> s
  | FunExpr (params, body) ->
      let params = String.concat params ~sep:", " in
      let body = to_string body in
      Printf.sprintf "(fun (%s) { %s })" params body
  | CallExpr (fun_expr, args) ->
      let fun_expr = to_string fun_expr in
      let args = String.concat (List.map args ~f:to_string) ~sep:", " in
      Printf.sprintf "(%s (%s))" fun_expr args
  | LetExpr (id_expr, right, in_expr) ->
      let id_expr = to_string id_expr in
      let right = to_string right in
      let in_expr = to_string in_expr in
      Printf.sprintf "([%s = %s] in %s)" id_expr right in_expr
