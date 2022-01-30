(* Test = a set of test cases. *)
type ('a, 'b) t

(* Make test. *)
val make :
  name:string -> equal:('a -> 'a -> bool) -> f:('b -> 'a) -> (string * 'a * 'b) list -> ('a, 'b) t

(* Run all test cases and print results. *)
val run_and_print : ('a, 'b) t -> to_string:('a -> string) -> unit

(* Uncurry. *)
module type IUncurry = sig
  (* 2 arguments. *)
  val arg2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

  (* 3 arguments. *)
  val arg3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd

  (* 4 arguments. *)
  val arg4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
end

module Uncurry : IUncurry
