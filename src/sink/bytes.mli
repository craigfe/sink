open Import

type t = bytes [@@implements Monoid.S, Eq.S, Ord.S, Blit.S, Show.S, Foldable.S]

val to_string : t -> string

val init : int -> (int -> char) -> bytes
