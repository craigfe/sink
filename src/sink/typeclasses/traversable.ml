module type S = sig
  type 'a t

  val traverse : [%b: 'f Applicative.t -> ('a -> 'a f) -> 'a t -> 'b t f]

  val sequence : [%b: 'm Monad.t -> 'a m t -> 'a t m]
end
[@@deriving typeclass]
