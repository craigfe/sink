open Sink_kernel

module type Fold_left = sig
  type ('a, 'p) t
  type 'a elt

  val fold_left : ('acc -> 'a elt -> 'acc) -> 'acc -> ('a, 'p) t -> 'acc

  val fold_until :
    ('acc -> 'a elt -> ('acc, 'b) either) ->
    'acc ->
    ('a, 'p) t ->
    ('acc, 'b) either
end

module type Fold_left_indexed = sig
  type ('a, 'p) t
  type 'a elt
  type index

  val fold_lefti :
    (index -> 'acc -> 'a elt -> 'acc) -> 'acc -> ('a, 'p) t -> 'acc

  val fold_untili :
    (index -> 'acc -> 'a elt -> ('acc, 'b) either) ->
    'acc ->
    ('a, 'p) t ->
    ('acc, 'b) either
end

module type Left_assoc = sig
  type ('a, 'p) t
  type 'a elt
  type index

  (** @inline *)
  include Fold_left with type ('a, 'p) t := ('a, 'p) t and type 'a elt := 'a elt

  (** @inline *)
  include
    Fold_left_indexed
      with type ('a, 'p) t := ('a, 'p) t
       and type 'a elt := 'a elt
       and type index := index

  val decons : ('a, 'p) t -> ('a elt * ('a, 'p) t) option
end

module type General = sig
  type ('a, 'p) t
  type 'a elt

  val reduce : 'm elt Monoid.t -> ('m, _) t -> 'm elt

  (** @inline *)
  include Fold_left with type ('a, 'p) t := ('a, 'p) t and type 'a elt := 'a elt

  val fold_right : ('a elt -> 'acc -> 'acc) -> ('a, _) t -> 'acc -> 'acc
  val iter : ('a elt -> unit) -> ('a, _) t -> unit

  val is_empty : ('a, _) t -> bool
  (** [is_empty t] is true iff [t] contains no elements. *)

  val length : ('a, _) t -> int
  (** [length t] is the number of elements contained in [t]. *)

  val exists : ('a elt -> bool) -> ('a, _) t -> bool
  (** [exists p t] is true iff at least one of the elements of [t] satisfies
      [p]. *)

  val for_all : ('a elt -> bool) -> ('a, _) t -> bool
  (** [for_all p t] returns true iff all of the elements of [t] satisfy [p]. *)

  val to_list : ('a, _) t -> 'a elt list
  val mem : 'a elt Eq.t -> 'a elt -> ('a, _) t -> bool
  val minimum : 'o elt Ord.t -> ('o, _) t -> 'o elt option
  val maximum : 'o elt Ord.t -> ('o, _) t -> 'o elt option
end

module type General_indexed = sig
  type ('a, 'p) t
  type 'a elt
  type index

  include General with type ('a, 'p) t := ('a, 'p) t and type 'a elt := 'a elt

  (** {2 Indexed operations} *)

  include
    Fold_left_indexed
      with type ('a, 'p) t := ('a, 'p) t
       and type 'a elt := 'a elt
       and type index := index

  val iteri : (index -> 'a elt -> unit) -> ('a, _) t -> unit
  val existsi : (index -> 'a elt -> bool) -> ('a, _) t -> bool
  val for_alli : (index -> 'a elt -> bool) -> ('a, _) t -> bool
end

module type S = sig
  type t
  type elt

  (** @inline *)
  include General with type (_, _) t := t and type 'a elt := elt
end

module type Indexed = sig
  type t
  type elt
  type index

  (** @inline *)
  include
    General_indexed
      with type (_, _) t := t
       and type 'a elt := elt
       and type index := index
end

module type S1 = sig
  type 'a t

  (** @inline *)
  include General with type ('a, _) t := 'a t and type 'a elt := 'a
end

module type Indexed1 = sig
  type 'a t
  type index

  (** @inline *)
  include
    General_indexed
      with type ('a, _) t := 'a t
       and type 'a elt := 'a
       and type index := index
end

module type Foldable = sig
  module type S = S
  module type S1 = S1
  module type General = General
  module type Indexed = Indexed
  module type Indexed1 = Indexed1

  module Of_left_assoc (X : Left_assoc) :
    General_indexed
      with type ('a, 'p) t := ('a, 'p) X.t
       and type 'a elt := 'a X.elt
       and type index := X.index

  module Of_indexable (X : Indexable.General) :
    General_indexed
      with type ('a, _) t := 'a X.t
       and type 'a elt := 'a X.elt
       and type index := int
end
