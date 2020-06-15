open Import

type t = unit [@@deriving branded]

let t = Repr.unit

let to_string () = "()"

let maximum = ()

let minimum = ()

module O = Ord.Of_stdlib_compare (struct
  type t = unit

  let compare _ _ = 0
end)

include O

module Infix = struct
  include Ord.Make_infix (struct
    type nonrec t = t

    include O
  end)
end
