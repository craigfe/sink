module type General = sig
  type 'a t
  type 'a elt

  val length : 'a t -> int
  val get : 'a t -> int -> 'a elt
end

module type Functional = sig
  include General
  (** @inline *)

  val sort : 'a elt Ord.t -> 'a t -> 'a t
end

module type Mutable = sig
  include General
  (** @inline *)

  val sort : 'a elt Ord.t -> 'a t -> unit
end

module type S = sig
  type t
  type elt

  (** @inline *)
  include General with type _ t := t and type _ elt := elt
end

module type Functional1 = sig
  include Functional with type 'a elt := 'a
  (** @inline *)
end

module type Mutable1 = sig
  include Mutable with type 'a elt := 'a
  (** @inline *)
end
