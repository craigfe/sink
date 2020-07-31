open Import

type t = bytes [@@implements Monoid.S, Eq.S, Ord.S, Blit.S, Show.S]

include Foldable.S with type t := t and type elt := char

val mem : char -> t -> bool
val minimum : t -> char option
val maximum : t -> char option
val to_string : t -> string
val init : int -> (int -> char) -> bytes
