open Import

type t = int

include Show.S with type t := t

include Eq.S with type t := t

include Ord.S with type t := t

include Num.S with type t := t

include Bounded.S with type t := t

module Infix : sig
  include Show.INFIX with type t := t

  include Eq.INFIX with type t := t

  include Ord.INFIX with type t := t

  include Num.INFIX with type t := t
end
