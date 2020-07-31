open Import

type dyn =
  | Opaque
  | Unit
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Bool of bool
  | String of string
  | Bytes of bytes
  | Char of char
  | Float of float
  | Option of dyn option
  | Either of (dyn, dyn) either
  | Result of (dyn, dyn) result
  | List of dyn list
  | Array of dyn array
  | Tuple of dyn list
  | Record of (string * dyn) list
  | Variant of string * dyn list
  | Map of (dyn * dyn) list
  | Set of dyn list

module type S = sig
  type t

  val to_dyn : t -> dyn
end

module type S1 = sig
  type 'a t

  val to_dyn : ('a -> dyn) -> 'a t -> dyn
end

module type Dyn = sig
  type t = dyn [@@implements Hashable.S]

  module Encoder : sig
    type dyn
    type 'a t = 'a -> dyn

    include Reifier.S with type 'a t := 'a t
    (** @inline *)

    val record : (string * dyn) list -> dyn
    val unknown : _ t
    val opaque : _ t
    val constr : string -> dyn list -> dyn
  end
  with type dyn := t

  module type S = S
  module type S1 = S1
end
