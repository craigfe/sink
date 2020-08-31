type nonrec 'a node = 'a Stdlib.Seq.node = Nil | Cons of 'a * 'a Stdlib.Seq.t

type nonrec 'a t = 'a Stdlib.Seq.t
[@@implements
  Monoid.S,
    Eq.S,
    Functor.S,
    Applicative.S,
    Monad.S,
    Foldable.S,
    Typeable.S,
    Higher.BRANDED]

val empty : unit -> 'a node
val cons : 'a -> 'a t -> unit -> 'a node
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val iter : ('a -> unit) -> 'a t -> unit
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b t
