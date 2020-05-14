type ('a, 'fn) app

module Newtype1 (T : sig
  type 'a t
end) =
struct
  type 'a t = 'a T.t

  type br

  external inj : 'a t -> ('a, br) app = "%identity"

  external prj : ('a, br) app -> 'a t = "%identity"
end

module type B1 = sig
  type 'a t

  type br

  val inj : 'a t -> ('a, br) app

  val prj : ('a, br) app -> 'a t
end
