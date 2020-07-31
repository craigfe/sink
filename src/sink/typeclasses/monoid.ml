(** Implementations of {!Monoid} must satisfy the following laws:

    - [empty <*> x] ≡ [x] (left identity)
    - [x <*> empty] ≡ [x] (right identity)
    - [(x <*> y) <*> z] ≡ [x <*> (y <*> z)] (associativity) *)
module type S = sig
  type t

  val empty : t
  val append : t -> t -> t [@@infix ( <*> )]
end
[@@deriving typeclass, infix, phantom { subderiving = infix }]
