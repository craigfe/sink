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

let set = A.set
let blit ~src ~src_pos ~dst ~dst_pos ~len = A.blit src src_pos dst dst_pos len
let to_dyn = Dyn.Encode.array
let to_list = A.to_list
let of_list = A.of_list
let to_array t = t
let zip _ = failwith "TODO"
let unzip _ = failwith "TODO"
let sort ord = Stdlib.Array.sort (Ord.to_int_compare ord)

module Matrix = struct
  type nonrec 'a t = 'a t t

  module Array = Stdlib.Array

  let make x_len y_len =
    if x_len <= 0 || y_len <= 0 then
      Format.kasprintf invalid_arg
        "Cannot create a matrix with dimensions (%d, %d)" x_len y_len;
    A.make_matrix x_len y_len

  let get t (i, j) = t.(i).(j)
  let set t (i, j) v = t.(i).(j) <- v
  let dimensions t = (A.length t, A.length t.(0))

  let transpose t =
    let x_len, y_len = dimensions t in
    let result = make y_len x_len t.(0).(0) in
    for i = 0 to x_len - 1 do
      for j = 0 to y_len - 1 do
        result.(j).(i) <- t.(i).(j)
      done
    done;
    result

  let pp pp_elt ppf t =
    let t = transpose t in
    let p fmt = Format.fprintf ppf fmt in
    let rows, columns = dimensions t in
    let padding = String.make (columns * 2) ' ' in

    p "@[<v 0>┌ %s┐@," padding;

    for row = 0 to rows - 1 do
      p "│ ";
      for column = 0 to columns - 1 do
        p "%a " pp_elt t.(row).(column)
      done;
      p "│@,"
    done;

    p "└ %s┘@]@,@." padding
end
