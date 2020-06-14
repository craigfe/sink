module type S = sig
  type t

  val equal : t -> t -> bool [@@infix ( = )]
end
[@@deriving typeclass, infix]

type 'a ty = 'a t

module type S1 = sig
  type 'a t

  val equal : 'a ty -> 'a t -> 'a t -> bool
end

module type S2 = sig
  type ('a, 'b) t

  val equal : 'a ty -> 'b ty -> ('a, 'b) t -> ('a, 'b) t -> bool
end
