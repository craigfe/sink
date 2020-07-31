module type General = sig
  type 'a t
  type 'a elt

  val length : 'a t -> int
  val get : 'a t -> int -> 'a elt
end
