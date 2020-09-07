open! Import

module type List = sig
  type index = int

  type 'a t = 'a list
  [@@implements
    Monoid.S,
      Eq.S,
      Foldable.Indexed { subst = (index, int) },
      Filterable.S { subst = (index, int) },
      Zippable.S,
      Functor.Indexed { subst = (index, int) },
      Applicative.S,
      Monad.S,
      Indexable.Functional,
      Typeable.S,
      Branded.S,
      Dyn.S]

  val hd : 'a t -> 'a option
  val hd_exn : 'a t -> 'a
  val tl : 'a t -> 'a t option
  val tl_exn : 'a t -> 'a t
  val nth : 'a t -> int -> 'a option
  val nth_exn : 'a t -> int -> 'a
  val init : int -> (int -> 'a) -> 'a t
  val rev : 'a t -> 'a t
  val rev_append : 'a t -> 'a t -> 'a t
  val sum : int t -> int
  val product : int t -> int
  val sequence_result : ('a, 'e) result t -> ('a t, 'e) result
  val distrib_result : ('a, 'e) result t -> ('a t, 'e t) result
  val take_while : ('a -> bool) -> 'a t -> 'a t
  val drop_while : ('a -> bool) -> 'a t -> 'a t
  val to_array : 'a t -> 'a array
  val of_array : 'a array -> 'a t

  module Assoc : sig
    type ('a, 'b) t = ('a * 'b) list

    val inverse : ('a, 'b) t -> ('b, 'a) t
  end
end
