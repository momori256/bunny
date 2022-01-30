open Base

(* Test case. *)
module type ICase = sig
  type ('a, 'b) t

  val make : string -> ('a -> 'a -> bool) -> 'a -> ('b -> 'a) -> 'b -> ('a, 'b) t
  val run : ('a, 'b) t -> bool * ('a * 'a) option
  val name : ('a, 'b) t -> string
end

module Case : ICase = struct
  type ('a, 'b) t = {
    name : string;
    equal : 'a -> 'a -> bool;
    expected : 'a;
    f : 'b -> 'a;
    input : 'b;
  }

  let make name equal expected f input = { name; equal; expected; f; input }

  let run case =
    let res = case.f case.input in
    let equal = case.equal case.expected res in
    let info = if equal then None else Some (case.expected, res) in
    (equal, info)

  let name case = case.name
end

(* Test. *)
type ('a, 'b) t = { name : string; cases : ('a, 'b) Case.t list }

let make ~name ~equal ~f cases =
  let cases = List.map cases ~f:(fun (name, exp, input) -> Case.make name equal exp f input) in
  { name; cases }

let run t = List.map t.cases ~f:(fun case -> (Case.name case, Case.run case))

let run_and_print t ~to_string =
  let open Stdio in
  let results = run t in
  let ok =
    let failed =
      List.exists results ~f:(fun (_, info) -> match info with true, None -> false | _ -> true)
    in
    not failed
  in

  let _ = printf "<%s> %s\n" t.name (if ok then "OK" else "Failed") in
  let _ =
    List.map results ~f:(fun (name, res) ->
        let info =
          match res with
          | true, None -> "OK"
          | false, Some (exp, r) ->
              Printf.sprintf "Failed [expected: %s, result: %s]" (to_string exp) (to_string r)
          | _ -> "Invalid result"
        in
        printf "\t(%s) %s\n" name info)
  in
  ()

(* Uncurrying. *)
module type IUncurry = sig
  val arg2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val arg3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
  val arg4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
end

module Uncurry : IUncurry = struct
  let arg2 f (x1, x2) = f x1 x2
  let arg3 f (x1, x2, x3) = f x1 x2 x3
  let arg4 f (x1, x2, x3, x4) = f x1 x2 x3 x4
end
