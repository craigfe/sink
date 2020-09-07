open! Import
module B = Stdlib.Bytes

module T = struct
  type t = bytes [@@deriving branded]

  let get = B.get
  let empty = B.empty
  let append = B.cat
  let compare = B.compare
  let blit ~src ~src_pos ~dst ~dst_pos ~len = B.blit src src_pos dst dst_pos len
  let to_string = B.to_string
  let init = B.init
  let length = B.length
end

include T

include Foldable.Of_indexable (struct
  include T

  type nonrec _ t = t
  type _ elt = char
end)

include Ord.Of_stdlib_compare (T)

let mem = mem (Char.eq Proxy.T)
let maximum = maximum (Char.ord Proxy.T)
let minimum = minimum (Char.ord Proxy.T)
