module type General = sig
  type 'a t
  type 'a elt

  val length : 'a t -> int
  val get : 'a t -> int -> 'a elt
end

module type S = sig
  type t
  type elt

  (** @inline *)
  include General with type _ t := t and type _ elt := elt
end

module type S1 = sig
  type 'a t

  (** @inline *)
  include General with type 'a t := 'a t and type 'a elt := 'a
end
