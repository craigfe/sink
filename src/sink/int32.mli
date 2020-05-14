open Import

type t = int32

include Show.S with type t := t

include Num.S with type t := t

module Infix : sig
  include Num.INFIX with type t := t
end
