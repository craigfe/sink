module type S = sig
  type t

  val t : t Repr.t
end

module type S1 = sig
  type 'a t

  val t : 'a Repr.t -> 'a t Repr.t
end
