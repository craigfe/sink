open Import
module I = Stdlib.Int32

type t = int32 [@@deriving branded]

let to_string = I.to_string

module N = struct
  let negate = I.neg
  let abs = I.abs
  let add = I.add
  let subtract = I.sub
  let multiply = I.mul
end

module Infix = Num.Make_infix (struct
  type nonrec t = t

  include N
end)

include N
