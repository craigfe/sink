module type S = sig
  type t

  val to_string : t -> string
end
[@@deriving typeclass, phantom]

module type INFIX = sig
  type t
end

module type Show = sig
  type nonrec 't t = 't t

  module type S = S
  module type S1 = S1
  module type INFIX = INFIX
end
