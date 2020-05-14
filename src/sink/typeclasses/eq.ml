module type S = sig
  type t

  val equal : t -> t -> bool
end
[@@deriving typeclass]

module type INFIX = sig
  type t

  val ( = ) : t -> t -> bool
end

module type S2 = sig
  type 'a t

  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
end
