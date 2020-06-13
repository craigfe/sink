module type S = sig
  type t

  val combine : t -> t -> t [@@infix ( <> )]
end
[@@deriving typeclass, infix]

module type S1 = sig
  type 'a t

  val combine : 'a t -> 'a t -> 'a t
end

module type INFIX1 = sig
  type 'a t

  val ( <> ) : 'a t -> 'a t -> 'a t
end
