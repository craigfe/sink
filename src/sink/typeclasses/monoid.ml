module type S = sig
  type t

  val empty : t

  val append : t -> t -> t [@@infix ( @ )]
end
[@@deriving typeclass, infix, phantom { subderiving = infix }]
