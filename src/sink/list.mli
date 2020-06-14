open Import

type 'a t = 'a list
[@@implements
  Semigroup.S,
    Eq.S,
    Foldable.S,
    Functor.S,
    Applicative.S,
    Monad.S,
    Typeable.S,
    Higher.BRANDED]

val is_empty : 'a list -> bool

val sequence_result : ('a, 'e) result list -> ('a list, 'e) result

val unzip : ('a * 'b) list -> 'a list * 'b list
