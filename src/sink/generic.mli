(** Generic functions built using the universal combinators from {!Repr} *)

val pp : 'a Repr.t -> Format.formatter -> 'a -> unit
val to_string : 'a Repr.t -> 'a -> string
val equal : 'a Repr.t -> 'a -> 'a -> bool
