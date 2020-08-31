type t = unit
[@@implements
  Show.S, Eq.S, Ord.S, Bounded.S, Enum.S, Typeable.S, Higher.BRANDED]

module Infix : sig
  include Eq.INFIX with type t := t
  include Ord.INFIX with type t := t
end
