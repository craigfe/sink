type t = int32 [@@implements Show.S, Num.S, Higher.BRANDED]

module Infix : sig
  include Num.INFIX with type t := t
end
