open Base

type t = Int of int | Bool of bool | Fun of Expression.t | Glet of (string * t)

let rec equal t1 t2 =
  match (t1, t2) with
  | Int x1, Int x2 -> Int.equal x1 x2
  | Bool x1, Bool x2 -> Bool.equal x1 x2
  | Fun x1, Fun x2 -> Expression.equal x1 x2
  | Glet (s1, t1), Glet (s2, t2) -> String.equal s1 s2 && equal t1 t2
  | _ -> false

let rec to_string = function
  | Int x -> Int.to_string x
  | Bool b -> Bool.to_string b
  | Fun expr -> Expression.to_string expr
  | Glet (k, v) -> Printf.sprintf "(%s -> %s)" k (to_string v)
