module type General = sig
  type 'a t
  type 'a elt
  type index

  val filter : ('a elt -> bool) -> 'a t -> 'a t
  val filter_map : ('a elt -> 'b elt option) -> 'a t -> 'b t
  val filteri : (int -> 'a elt -> bool) -> 'a t -> 'a t
  val filter_mapi : (int -> 'a elt -> 'b elt option) -> 'a t -> 'b t
end

module type S1 = sig
  type 'a t
  type index

  (** @inline *)
  include
    General with type 'a t := 'a t and type 'a elt := 'a and type index := index
end
