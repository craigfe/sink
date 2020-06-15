open Import

type 'a t = 'a array
[@@implements
  Semigroup.S,
    Functor.S,
    Foldable.S,
    Zippable.S,
    Blit.S,
    Typeable.S,
    Higher.BRANDED,
    Dyn.S]

val init : int -> (int -> 'a) -> 'a t
