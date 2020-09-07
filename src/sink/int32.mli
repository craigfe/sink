open! Import

type t = int32 [@@implements Show.S, Num.S, Branded.S]

module Infix : sig
  include Num.INFIX with type t := t
end
