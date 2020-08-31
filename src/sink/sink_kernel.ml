(** The canonical definitions of various standard types: *)

type empty = |
type ('a, 'b) either = Left of 'a | Right of 'b
