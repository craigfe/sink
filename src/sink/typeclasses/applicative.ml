module type S = sig
  type 'a t

  val pure : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val seq : 'a t -> 'b t -> 'b t
end
[@@deriving typeclass]

module type INFIX = sig
  type 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( *> ) : 'a t -> 'b t -> 'b t
end
