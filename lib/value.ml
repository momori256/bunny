open Base

type t = Integer of int | Boolean of bool | Function of Expression.t

let to_string = function
  | Integer x -> Int.to_string x
  | Boolean b -> Bool.to_string b
  | Function expr -> Expression.to_string expr