open Import
module I = Stdlib.Int

type t = int [@@deriving branded]

let to_string = I.to_string

let maximum = I.max_int

let minimum = I.min_int

let eq = Eq.poly

let ord = Ord.poly

module N : Num.S with type t := t = struct
  let negate = I.neg

  let abs = I.abs

  let add = I.add

  let subtract = I.sub

  let multiply = I.mul
end

include N
module O = Ord.Of_stdlib_compare (I)
include O

module Infix = struct
  include Num.Make_infix (struct
    type nonrec t = t

    include N
  end)

  include Ord.Make_infix (struct
    type nonrec t = t

    include O
  end)
end
