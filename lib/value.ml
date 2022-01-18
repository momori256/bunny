type t = Integer of int | Boolean of bool

let to_string = function Integer x -> Int.to_string x | Boolean b -> Bool.to_string b
