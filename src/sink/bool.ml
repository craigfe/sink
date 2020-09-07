open! Import
module B = Stdlib.Bool

type t = bool [@@deriving branded]

let minimum = false
let maximum = true
let all = [ false; true ]

external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
external to_int : bool -> int = "%identity"

let to_float = function false -> 0. | true -> 1.
let to_string = function false -> "false" | true -> "true"

module Forall = struct
  let empty = true
  let append = ( && )
end

module Exists = struct
  let empty = false
  let append = ( || )
end

module O = Ord.Of_stdlib_compare (B)
include O

module Infix = struct
  include Ord.Make_infix (struct
    type nonrec t = t

    include O
  end)
end
