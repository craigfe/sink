open Import

type t = empty = | [@@deriving branded]

let t = Repr.empty
let absurd = function (_ : t) -> .
let all = []
