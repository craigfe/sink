(** The canonical definitions of various standard types: *)

let ( >> ) f g x = g (f x)

type empty = |
type ('a, 'b) either = Left of 'a | Right of 'b

let absurd : type a. empty -> a = function _ -> .
