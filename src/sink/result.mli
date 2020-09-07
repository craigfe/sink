open! Import

type ('a, 'e) t = ('a, 'e) result [@@implements Functor.S, Monad.S, Branded.S]

val errorf :
  ('a, Format.formatter, unit, ('b, [> `Msg of string ]) t) format4 -> 'a

module Infix : sig
  val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( >>| ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val ( >=> ) : ('a -> ('b, 'c) t) -> ('b -> ('d, 'c) t) -> 'a -> ('d, 'c) t
end

module Syntax : sig
  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
end
