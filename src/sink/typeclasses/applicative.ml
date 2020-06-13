module type S = sig
  type 'a t

  val pure : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t [@@infix ( <*> )]

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val seq : 'a t -> 'b t -> 'b t [@@infix ( *> )]
end
[@@deriving typeclass, infix]

module type S1 = S
