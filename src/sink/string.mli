open Import

type t = string
[@@implements
  Monoid.S,
    Eq.S,
    Blit.S,
    Functor.S { subst = (elt, char) },
    Foldable.S { subst = (elt, char) },
    Zippable.S { subst = (elt, char) }]
