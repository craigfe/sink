type ('a, 'b) t = 'a * 'b

val v : 'a -> 'b -> ('a, 'b) t
val fst : ('a, _) t -> 'a
val snd : (_, 'b) t -> 'b
val flip : ('a, 'b) t -> ('b, 'a) t
val map : ('a -> 'b) -> ('a, 'a) t -> ('b, 'b) t
val map1 : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
val map2 : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t

module Fst : sig end
