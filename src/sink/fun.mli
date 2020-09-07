open! Import

type ('a, 'b) t = 'a -> 'b [@@implements Branded.S]

val id : 'a -> 'a
val const : 'a -> _ -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val negate : ('a -> bool) -> 'a -> bool
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
