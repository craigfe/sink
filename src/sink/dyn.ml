include Dyn_intf

type t = dyn

module Encoder = struct
  type 'a t = 'a -> dyn

  let unit () = Unit

  let char x = Char x

  let string x = String x

  let int x = Int x

  let float x = Float x

  let bool x = Bool x

  let pair f g (x, y) = Tuple [ f x; g y ]

  let triple f g h (x, y, z) = Tuple [ f x; g y; h z ]

  let list f l = List (List.map f l)

  let array f a = Array (Array.map f a)

  let option f x = Option (match x with None -> None | Some x -> Some (f x))

  let record r = Record r

  let unknown _ = String "<unknown>"

  let opaque _ = String "<opaque>"

  let constr s args = Variant (s, args)
end

let hash = Stdlib.Hashtbl.hash
