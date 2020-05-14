module type S = sig
  type t

  val negate : t -> t

  val abs : t -> t

  val add : t -> t -> t

  val subtract : t -> t -> t

  val multiply : t -> t -> t
end
[@@deriving typeclass]

module type INFIX = sig
  type t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t
end

module type Num = sig
  module type S = S

  module type INFIX = INFIX

  module Make_infix (N : S) : INFIX with type t := N.t
end
