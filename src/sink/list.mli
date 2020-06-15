open Import

type 'a t = 'a list
[@@implements
  Monoid.S,
    Eq.S,
    Foldable.S,
    Zippable.S,
    Functor.S,
    Applicative.S,
    Monad.S,
    Typeable.S,
    Higher.BRANDED,
    Dyn.S]

val init : int -> (int -> 'a) -> 'a t

val rev : 'a t -> 'a t

val sequence_result : ('a, 'e) result t -> ('a t, 'e) result

val take_while : ('a -> bool) -> 'a t -> 'a t

val drop_while : ('a -> bool) -> 'a t -> 'a t
