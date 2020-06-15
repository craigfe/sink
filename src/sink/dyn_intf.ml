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
  | List of dyn list
  | Array of dyn array
  | Tuple of dyn list
  | Record of (string * dyn) list
  | Variant of string * dyn list
  | Map of (dyn * dyn) list
  | Set of dyn list
[@@implements Hashable.S]

module type S = sig
  type t

  val to_dyn : t -> dyn
end

module type S1 = sig
  type 'a t

  val to_dyn : dyn t -> dyn
end

module type Dyn = sig
  type t = dyn

  module Encoder : sig
    type dyn

    type 'a t = 'a -> dyn

    val unit : unit t

    val char : char t

    val string : string t

    val int : int t

    val float : float t

    val bool : bool t

    val pair : 'a t -> 'b t -> ('a * 'b) t

    val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

    val list : 'a t -> 'a list t

    val array : 'a t -> 'a array t

    val option : 'a t -> 'a option t

    val record : (string * dyn) list -> dyn

    val unknown : _ t

    val opaque : _ t

    val constr : string -> dyn list -> dyn
  end
  with type dyn := t

  module type S = S

  module type S1 = S1
end
