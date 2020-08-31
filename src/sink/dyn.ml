open Sink_kernel
include Dyn_intf

type t = dyn

module Encoder = struct
  type 'a t = 'a -> dyn

  let unit () = Unit
  let char x = Char x
  let string x = String x
  let int x = Int x
  let int32 x = Int32 x
  let int64 x = Int64 x
  let float x = Float x
  let bool x = Bool x
  let pair f g (x, y) = Tuple [ f x; g y ]
  let triple f g h (x, y, z) = Tuple [ f x; g y; h z ]
  let list f l = List (Stdlib.List.map f l)
  let array f a = Array (Stdlib.Array.map f a)
  let option f x = Option (match x with None -> None | Some x -> Some (f x))

  let either f g x =
    Either (match x with Left x -> Left (f x) | Right x -> Right (g x))

  let result f g x =
    Result (match x with Ok x -> Ok (f x) | Error x -> Error (g x))

  let record r = Record r
  let unknown _ = String "<unknown>"
  let opaque _ = String "<opaque>"
  let constr s args = Variant (s, args)
end

let hash = Stdlib.Hashtbl.hash
