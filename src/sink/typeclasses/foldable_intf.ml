module type MINIMAL1 = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module type S = sig
  type t

  type elt

  val fold_left : ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc

  val fold_right : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val iter : (elt -> unit) -> t -> unit

  val is_empty : t -> bool

  val length : t -> int

  val mem : elt -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val forall : (elt -> bool) -> t -> bool

  val maximum : elt Ord.t -> t -> elt option

  val minimum : elt Ord.t -> t -> elt option

  val to_list : t -> elt list

  val to_array : t -> elt array
end

module type S1 = sig
  type 'a t

  val fold : 'm Monoid.t -> 'm t -> 'm

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

  val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val iter : ('a -> unit) -> 'a t -> unit

  val is_empty : 'a t -> bool

  val length : 'a t -> int

  val mem : 'a Eq.t -> 'a -> 'a t -> bool

  val exists : ('a -> bool) -> 'a t -> bool

  val forall : ('a -> bool) -> 'a t -> bool

  val maximum : 'a Ord.t -> 'a t -> 'a option

  val minimum : 'a Ord.t -> 'a t -> 'a option

  val to_list : 'a t -> 'a list

  val to_array : 'a t -> 'a array
end
[@@deriving typeclass]

module type Of_indexed_arg = sig
  type 'a t

  type 'a elt

  val length : 'a t -> int

  val get : 'a t -> int -> 'a elt
end

module type Foldable = sig
  module type S = S

  module type S1 = S1

  module Make_default (X : MINIMAL1) : S1 with type 'a t := 'a X.t

  module Of_indexed (X : Of_indexed_arg) : sig
    open X

    val fold : 'm elt Monoid.t -> 'm t -> 'm elt

    val fold_left : ('acc -> 'a elt -> 'acc) -> 'acc -> 'a t -> 'acc

    val fold_right : ('a elt -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

    val iter : ('a elt -> unit) -> 'a t -> unit

    val is_empty : 'a t -> bool

    val length : 'a t -> int

    val to_list : 'a t -> 'a elt list

    val to_array : 'a t -> 'a elt array

    val exists : ('a elt -> bool) -> 'a t -> bool

    val forall : ('a elt -> bool) -> 'a t -> bool

    val mem : 'a elt Eq.t -> 'a elt -> 'a t -> bool

    val minimum : 'a elt Ord.t -> 'a t -> 'a elt option

    val maximum : 'a elt Ord.t -> 'a t -> 'a elt option
  end
end
