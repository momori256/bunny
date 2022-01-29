open Base
module T = Token
module E = Expression
module V = Value
module Env = Environment

let rec eval expr lenv genv =
  match expr with
  | E.Int x -> V.Int x
  | E.Bool b -> V.Bool b
  | E.Prefix (op, expr) -> (
      match op with
      | T.Plus -> eval expr lenv genv
      | T.Minus -> (
          match eval expr lenv genv with
          | V.Int x -> V.Int (-x)
          | V.Bool _ | V.Fun _ | V.Glet _ -> failwith "type error")
      | T.Tilde -> (
          match eval expr lenv genv with
          | V.Int _ | V.Fun _ | V.Glet _ -> failwith (Printf.sprintf "type error")
          | V.Bool b -> V.Bool (not b))
      | _ -> failwith (Printf.sprintf "Illegal prefix operator (%s)" (T.to_string op)))
  | E.Infix (op, expr1, expr2) as expr -> (
      let v1 = eval expr1 lenv genv in
      let v2 = eval expr2 lenv genv in
      match (op, v1, v2) with
      | T.Plus, V.Int x1, V.Int x2 -> V.Int (x1 + x2)
      | T.Minus, V.Int x1, V.Int x2 -> V.Int (x1 - x2)
      | T.Star, V.Int x1, V.Int x2 -> V.Int (x1 * x2)
      | T.Less, V.Int x1, V.Int x2 -> V.Bool (x1 < x2)
      | T.Greater, V.Int x1, V.Int x2 -> V.Bool (x1 > x2)
      | T.Equal, V.Int x1, V.Int x2 -> V.Bool (x1 = x2)
      | T.Equal, V.Bool b1, V.Bool b2 -> V.Bool (Bool.equal b1 b2)
      | T.Nequal, V.Int x1, V.Int x2 -> V.Bool (x1 <> x2)
      | T.Nequal, V.Bool b1, V.Bool b2 -> V.Bool (not (Bool.equal b1 b2))
      | _ -> failwith (Printf.sprintf "Illegal infix Expr (%s)" (E.to_string expr)))
  | E.If (cond, conq, alt) -> (
      match eval cond lenv genv with
      | V.Bool b -> if b then eval conq lenv genv else eval alt lenv genv
      | _ -> failwith "condition must be boolean")
  | E.Ident s -> (
      match Env.find lenv s with
      | Some v -> v
      | None -> (
          match Env.find genv s with
          | Some v -> v
          | None -> failwith (Printf.sprintf "Undefined E.Identifier (%s)" s)))
  | E.Fun _ as expr -> V.Fun expr
  | E.Call (expr, args) -> (
      match expr with
      | E.Fun (params, body) ->
          let args = List.map args ~f:(fun arg -> eval arg lenv genv) in
          let new_env =
            List.map2_exn params args ~f:(fun p a -> (p, a))
            |> List.fold ~init:lenv ~f:(fun acc (p, a) -> Env.add acc ~key:p ~value:a)
          in
          eval body new_env genv
      | E.Ident _ -> (
          match eval expr lenv genv with
          | V.Fun fun_expr -> eval (E.Call (fun_expr, args)) lenv genv
          | _ -> failwith "type error")
      | _ -> failwith "type error")
  | E.Let (ident, rhs, body) -> (
      match ident with
      | E.Ident s -> (
          let rhs = eval rhs lenv genv in
          match body with
          | Some body -> eval body (Env.add lenv ~key:s ~value:rhs) genv
          | None -> V.Glet (s, rhs))
      | _ -> failwith "type error")

let eval_string expr = V.to_string (eval expr Env.empty Env.empty)
