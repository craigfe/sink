open Import

module type S = sig
  type t

  type elt

  val zip_with : (elt -> elt -> elt) -> t -> t -> t

  val unzip_with : (elt -> elt * elt) -> t -> t * t

  val partition : (elt -> bool) -> t -> t * t

  val partition_map : (elt -> (elt, elt) either) -> t -> t * t
end

module type S1 = sig
  type 'a t

  val zip : 'a t -> 'b t -> ('a * 'b) t

  val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val unzip : ('a * 'b) t -> 'a t * 'b t

  val unzip_with : ('a -> 'b * 'c) -> 'a t -> 'b t * 'c t

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

  val partition_map : ('a -> ('b, 'c) either) -> 'a t -> 'b t * 'c t
end
[@@deriving typeclass, infix, phantom { subderiving = infix }]
