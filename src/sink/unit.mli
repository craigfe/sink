open Import

type t = unit

include Bounded.S with type t := t

include Show.S with type t := t

include Eq.S with type t := t

include Ord.S with type t := t

module Infix : sig
  include Eq.INFIX with type t := t

  include Ord.INFIX with type t := t
end
