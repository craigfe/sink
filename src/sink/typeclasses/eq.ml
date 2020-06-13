module type S = sig
  type t

  val equal : t -> t -> bool [@@infix ( = )]
end
[@@deriving typeclass, infix]

module type S1 = sig
  type 'a t

  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
end
