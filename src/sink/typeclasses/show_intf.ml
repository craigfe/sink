module type S = sig
  type t

  val to_string : t -> string
end
[@@deriving typeclass]

module type S1 = sig
  type 'a t

  val to_string : 'a t -> string
end

module type INFIX = sig
  type t
end

module type Show = sig
  module type S = S

  module type S1 = S1

  module type INFIX = INFIX
end
