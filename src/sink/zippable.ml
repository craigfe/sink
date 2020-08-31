open Sink_kernel

module type General = sig
  type 'a t
  type 'a elt

  val zip_with : ('a elt -> 'b elt -> 'c elt) -> 'a t -> 'b t -> 'c t
  val unzip_with : ('a elt -> 'b elt * 'c elt) -> 'a t -> 'b t * 'c t
  val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t
  val partition_map : ('a elt -> ('b elt, 'c elt) either) -> 'a t -> 'b t * 'c t
end

module type S = sig
  type t
  type elt

  (** @inline *)
  include General with type _ t := t and type _ elt := elt
end

module type S1 = sig
  type 'a t

  val zip : 'a t -> 'b t -> ('a * 'b) t
  val unzip : ('a * 'b) t -> 'a t * 'b t

  (** @inline *)
  include General with type 'a t := 'a t and type 'a elt := 'a
end

module Of_indexable (X : Indexable.General) :
  General with type 'a t := 'a X.t and type 'a elt := 'a X.elt = struct
  let zip_with _ = failwith "TODO"
  let unzip_with _ = failwith "TODO"
  let partition _ = failwith "TODO"
  let partition_map _ = failwith "TODO"
end

(* [@@deriving typeclass, infix, phantom { subderiving = infix }] *)
