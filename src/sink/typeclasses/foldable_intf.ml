module type MINIMAL = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module type S1 = sig
  type 'a t

  val fold : 'm Monoid.t -> 'm t -> 'm

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

  val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val null : 'a t -> bool

  val length : 'a t -> int

  val mem : 'a Eq.t -> 'a -> 'a t -> bool

  val maximum : 'a Ord.t -> 'a t -> 'a option

  val minimum : 'a Ord.t -> 'a t -> 'a option

  val sum : int t -> int

  val product : int t -> int
end
[@@deriving typeclass]

module type Foldable = sig
  module type S1 = S1

  module Make_default (X : MINIMAL) : S1 with type 'a t := 'a X.t
end
