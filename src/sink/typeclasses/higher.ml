open T

type ('a, 'fn) app

type ('b, 'a, 'fn) app2 = ('b, ('a, 'fn) app) app

module type BRANDED = sig
  type t

  type br = t
end

module type BRANDED1 = sig
  type 'a t

  type br

  external inj : 'a t -> ('a, br) app = "%identity"

  external prj : ('a, br) app -> 'a t = "%identity"
end

module type BRANDED2 = sig
  type ('a, 'b) t

  type br

  external inj : ('a, 'b) t -> ('a, 'b, br) app2 = "%identity"

  external prj : ('a, 'b, br) app2 -> ('a, 'b) t = "%identity"
end

module Newtype (T : T) : BRANDED with type t := T.t = struct
  type br = T.t
end

module Newtype1 (T : T1) : BRANDED1 with type 'a t := 'a T.t = struct
  type br

  external inj : 'a T.t -> ('a, br) app = "%identity"

  external prj : ('a, br) app -> 'a T.t = "%identity"
end

module Newtype2 (T : T2) : BRANDED2 with type ('a, 'b) t := ('a, 'b) T.t =
struct
  type br

  external inj : ('a, 'b) T.t -> ('a, 'b, br) app2 = "%identity"

  external prj : ('a, 'b, br) app2 -> ('a, 'b) T.t = "%identity"
end
