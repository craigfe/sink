open Higher
open Sink_kernel

module type S = sig
  type 'a t [@@implements Higher.BRANDED]

  val empty : empty t
  val unit : unit t
  val bool : bool t
  val char : char t
  val string : string t
  val bytes : bytes t

  val int : int t
  (** Numeric types: *)

  val nativeint : nativeint t
  val int32 : int32 t
  val int64 : int64 t
  val float : float t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** Container types: *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val option : 'a t -> 'a option t
  val either : 'a t -> 'b t -> ('a, 'b) either t
  val result : 'a t -> 'e t -> ('a, 'e) result t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
  val lazy_ : 'a t -> 'a Stdlib.Lazy.t t
end

module type Repr = sig
  module type S = S
  (** This module type defines the structure of the standard OCaml types (for
      instance, capturing that the list type). Implementations of this module
      type, known as {i interpreter}s, represent *)

  include S
  (** The universal interpreter. Terms built against this language can be run
      against arbitrary interpreters of the syntax {!S} supplied at runtime. *)

  type 'br generic = { generic : 'a. 'a t -> ('a, 'br) app } [@@unboxed]

  val make : (module S with type br = 'br) -> 'br generic
  val pp : Format.formatter -> _ t -> unit
  val to_string : _ t -> string
end
