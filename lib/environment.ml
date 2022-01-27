open Base

type t = (string * Expression.t) list

let empty = []

(* Add key value pair. *)
let add t ~key ~value = List.Assoc.add t key value ~equal:String.equal

(* Find item. *)
let find t key = List.Assoc.find_exn t key ~equal:String.equal
