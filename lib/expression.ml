open Base
module T = Token

type operator = T.t

type t =
  | Int of int
  | Bool of bool
  | Prefix of operator * t
  | Infix of operator * t * t
  | If of t * t * t
  | Ident of string
  | Fun of string list * t
  | Call of t * t list
  | Let of t * t * t option
(* | SuffixExpr of operator * Expr *)

let rec equal t1 t2 =
  match (t1, t2) with
  | Int x1, Int x2 -> x1 = x2
  | Bool b1, Bool b2 -> Bool.equal b1 b2
  | Prefix (op1, expr1), Prefix (op2, expr2) -> T.equal op1 op2 && equal expr1 expr2
  | Infix (op1, l1, r1), Infix (op2, l2, r2) -> T.equal op1 op2 && equal l1 l2 && equal r1 r2
  | If (cn1, co1, al1), If (cn2, co2, al2) -> equal cn1 cn2 && equal co1 co2 && equal al1 al2
  | Ident s1, Ident s2 -> String.equal s1 s2
  | Fun (args1, bd1), Fun (args2, bd2) -> List.equal String.equal args1 args2 && equal bd1 bd2
  | Call (f1, p1), Call (f2, p2) -> equal f1 f2 && List.equal equal p1 p2
  | _, _ -> false

let rec to_string = function
  | Int x -> Int.to_string x
  | Bool b -> Bool.to_string b
  | Prefix (op, expr) -> Printf.sprintf "(%s%s)" (T.to_string op) (to_string expr)
  | Infix (op, expr1, expr2) ->
      let op = T.to_string op in
      let expr1 = to_string expr1 in
      let expr2 = to_string expr2 in
      Printf.sprintf "(%s %s %s)" expr1 op expr2
  | If (cond, cons, alt) ->
      let cond = to_string cond in
      let conq = to_string cons in
      let alt = to_string alt in
      Printf.sprintf "(if (%s) then (%s) else (%s))" cond conq alt
  | Ident s -> s
  | Fun (params, body) ->
      let params = String.concat params ~sep:", " in
      let body = to_string body in
      Printf.sprintf "(fun (%s) { %s })" params body
  | Call (fun_expr, args) ->
      let fun_expr = to_string fun_expr in
      let args = String.concat (List.map args ~f:to_string) ~sep:", " in
      Printf.sprintf "(%s (%s))" fun_expr args
  | Let (id_expr, right, in_expr) -> (
      let id_expr = to_string id_expr in
      let right = to_string right in
      match in_expr with
      | Some in_expr ->
          let in_expr = to_string in_expr in
          Printf.sprintf "([%s = %s] in %s)" id_expr right in_expr
      | None -> Printf.sprintf "([%s = %s])" id_expr right)
