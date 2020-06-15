open Import
module C = Stdlib.Char
module S = Stdlib.String

module T = struct
  type t = string

  let empty = ""

  let append a b = S.concat a [ b ]

  let equal = S.equal

  let map = S.map

  let get = S.get

  let length = S.length
end

include T

include Foldable.Of_indexed (struct
  include T

  type nonrec _ t = t

  type _ elt = char
end)
