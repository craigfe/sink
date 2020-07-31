open Import
module A = Stdlib.Array

module T = struct
  type 'a t = 'a array [@@deriving branded]
  type 'a elt = 'a

  (* let forall2_exn f t1 t2 =
   *   let len = A.length t1 in
   *   if A.length t2 <> len then invalid_arg "Array.for_all2_exn";
   *   let rec inner i =
   *     if i < 0 then true else f (A.get t1 i) (A.get t2 i) && inner (i - 1)
   *   in
   *   inner (len - 1) *)

  let combine = A.append
  let get = A.get
  let length = A.length
  let t = Repr.array
  let init = A.init
  let map = A.map
end

include T
include Foldable.Of_indexable (T)
include Zippable.Of_indexable (T)

let blit ~src ~src_pos ~dst ~dst_pos ~len = A.blit src src_pos dst dst_pos len
let to_dyn = Dyn.Encoder.array
let to_list = A.to_list
let to_array t = t
let zip _ = failwith "TODO"
let unzip _ = failwith "TODO"
