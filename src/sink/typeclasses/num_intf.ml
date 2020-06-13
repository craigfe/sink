module type S = sig
  type t

  val negate : t -> t

  val abs : t -> t

  val add : t -> t -> t [@@infix ( + )]

  val subtract : t -> t -> t [@@infix ( - )]

  val multiply : t -> t -> t [@@infix ( * )]
end
[@@deriving typeclass, infix]

module type Num = sig
  module type S = S

  module type INFIX = INFIX

  module Make_infix (N : S) : INFIX with type t := N.t
end
