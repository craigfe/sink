module type S = sig
  type t

  type elt

  val map : (elt -> elt) -> t -> t
end

module type S1 = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end
[@@deriving typeclass]

module type INFIX = sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix alias of {!S.map}. *)
end

module type INFIX2 = sig
  type ('a, 'e) t

  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  (** Infix alias of {!S.map}. *)
end

module type SYNTAX = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Syntax alias of {!S.map}. *)
end

module type EXT = sig
  include S1

  module Infix : INFIX with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t
end
