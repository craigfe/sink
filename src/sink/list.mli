open Import

type index = int

type 'a t = 'a list
[@@implements
  Monoid.S,
    Eq.S,
    Foldable.Indexed { subst = (index, int) },
    Zippable.S,
    Functor.Indexed { subst = (index, int) },
    Applicative.S,
    Monad.S,
    Typeable.S,
    Higher.BRANDED,
    Dyn.S]

val init : int -> (int -> 'a) -> 'a t
val rev : 'a t -> 'a t
val sum : int t -> int
val product : int t -> int
val sequence_result : ('a, 'e) result t -> ('a t, 'e) result
val take_while : ('a -> bool) -> 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val to_array : 'a t -> 'a array
