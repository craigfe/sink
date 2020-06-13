open Import

type 'a t = 'a list
[@@implements
  Semigroup.S, Foldable.S, Functor.S, Applicative.S, Monad.S, Typeable.S]

val is_empty : 'a list -> bool
