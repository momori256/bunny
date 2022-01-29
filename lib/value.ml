open Base

type t = Int of int | Bool of bool | Fun of Expression.t | Glet of (string * t)

let rec to_string = function
  | Int x -> Int.to_string x
  | Bool b -> Bool.to_string b
  | Fun expr -> Expression.to_string expr
  | Glet (k, v) -> Printf.sprintf "(%s -> %s)" k (to_string v)
