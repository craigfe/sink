open Import

type t = string
[@@implements
  Monoid.S,
    Eq.S,
    Blit.S,
    Functor.S { subst = (elt, char) },
    Indexable.S { subst = (elt, char) },
    Foldable.S { subst = (elt, char) },
    Zippable.S { subst = (elt, char) }]

val make : int -> char -> string
val to_list : string -> char list
val of_list : char list -> string
val split_on : char -> string -> string list
