open Import

type t = unit
[@@implements Bounded.S, Show.S, Eq.S, Ord.S, Typeable.S, Higher.BRANDED]

module Infix : sig
  include Eq.INFIX with type t := t

  include Ord.INFIX with type t := t
end
