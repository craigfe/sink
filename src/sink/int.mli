open Import

type t = int
[@@implements Show.S, Eq.S, Ord.S, Num.S, Bounded.S, Higher.BRANDED]

module Infix : sig
  include Show.INFIX with type t := t

  include Eq.INFIX with type t := t

  include Ord.INFIX with type t := t

  include Num.INFIX with type t := t
end
