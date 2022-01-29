open Base

type t = Integer of int | Boolean of bool | Function of Expression.t | AddGlobal of (string * t)

let rec to_string = function
  | Integer x -> Int.to_string x
  | Boolean b -> Bool.to_string b
  | Function expr -> Expression.to_string expr
  | AddGlobal (k, v) -> Printf.sprintf "(%s -> %s)" k (to_string v)
