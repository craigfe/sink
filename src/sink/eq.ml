module type S = sig
  type t

  val equal : t -> t -> bool [@@infix ( = )]
end
[@@deriving typeclass, infix]

type 'a ty = 'a t

let poly (type a) (Proxy.T : a Proxy.t) : a t = { equal = Stdlib.( = ) }

module type S1 = sig
  type 'a t

  val equal : 'a ty -> 'a t -> 'a t -> bool
end

module type S2 = sig
  type ('a, 'b) t

  val equal : 'a ty -> 'b ty -> ('a, 'b) t -> ('a, 'b) t -> bool
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val equal :
    'a ty -> 'b ty -> 'c ty -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val equal :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    ('a, 'b, 'c, 'd) t ->
    ('a, 'b, 'c, 'd) t ->
    bool
end
