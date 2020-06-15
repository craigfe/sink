open Import
module A = Stdlib.Array

module T = struct
  type 'a t = 'a array [@@deriving branded]

  (* let forall2_exn f t1 t2 =
   *   let len = A.length t1 in
   *   if A.length t2 <> len then invalid_arg "Array.for_all2_exn";
   *   let rec inner i =
   *     if i < 0 then true else f (A.get t1 i) (A.get t2 i) && inner (i - 1)
   *   in
   *   inner (len - 1) *)

  let combine = A.append

  let get = A.get

  let fold (type m) (m : m Monoid.t) = A.fold_left m.append m.empty

  let length = A.length

  let minimum _ = failwith "TODO"

  let blit = A.blit

  let t _ = failwith "TODO"

  let init = A.init

  let to_list = A.to_list

  let to_array t = t

  let iter = A.iter
end

include T

include Foldable.Of_indexed (struct
  include T

  type 'a elt = 'a
end)

let to_array t = t
