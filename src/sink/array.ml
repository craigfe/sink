open Import
module A = Stdlib.Array

type 'a t = 'a array [@@deriving branded]

(* let forall f t =
 *   let rec inner i = if i < 0 then true else f (A.get t i) && inner (i - 1) in
 *   inner (A.length t - 1) *)

let exists f t =
  let rec inner i = if i < 0 then false else f (A.get t i) || inner (i - 1) in
  inner (A.length t - 1)

(* let forall2_exn f t1 t2 =
 *   let len = A.length t1 in
 *   if A.length t2 <> len then invalid_arg "Array.for_all2_exn";
 *   let rec inner i =
 *     if i < 0 then true else f (A.get t1 i) (A.get t2 i) && inner (i - 1)
 *   in
 *   inner (len - 1) *)

include (
  struct
    let combine = A.append
  end :
    Semigroup.S1 with type 'a t := 'a t )

include (
  struct
    let fold (type m) (m : m Monoid.t) = A.fold_left m.append m.empty

    let fold_left = A.fold_left

    let fold_right = A.fold_right

    let null i = A.length i = 0

    let length = A.length

    let mem Eq.{ equal } elt t = exists (equal elt) t

    let maximum _ = failwith "TODO"

    let minimum _ = failwith "TODO"

    let product = fold_left ( * ) 1

    let sum = fold_left ( + ) 0
  end :
    Foldable.S1 with type 'a t := 'a t )

let t _ = failwith "TODO"
