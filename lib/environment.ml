open Base
module V = Value

type t = V.t list

let empty = []

(* Add key value pair. *)
let add t ~key ~value = (key, value) :: t

(* Find item. *)
let find t key =
  match List.find t ~f:(fun (k, _) -> String.equal k key) with
  | Some (_, v) -> Some v
  | None -> None

let to_string t =
  List.map t ~f:(fun (key, value) -> Printf.sprintf "(%s -> %s)" key (Expression.to_string value))
  |> String.concat ~sep:", "
