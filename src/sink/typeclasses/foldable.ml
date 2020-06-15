include Foldable_intf

module Make_default (X : MINIMAL1) : S1 with type 'a t := 'a X.t = struct
  let fold_left = X.fold_left

  let fold (type m) ({ append; empty } : m Monoid.t) = fold_left append empty

  (* let fold_right =  *)

  let is_empty l = fold_left (fun _ _ -> false) true l

  let mem (type a) ({ equal } : a Eq.t) elt =
    fold_left (fun a x -> a || equal x elt) false

  let iter _ = failwith "TODO"

  let to_list _ = failwith "TODO"

  let to_array _ = failwith "TODO"

  let minimum _ = failwith "TODO"

  let maximum _ = failwith "TODO"

  let fold_right _ = failwith "TODO"

  let length _ = failwith "TODO"

  let forall _ = failwith "TODO"

  let exists _ = failwith "TODO"
end

module Of_indexed (X : Of_indexed_arg) = struct
  open X

  let length = length

  let is_empty t = length t = 0

  let iter f t =
    for i = 0 to length t - 1 do
      f (get t i)
    done

  let fold_left f acc t =
    let acc = ref acc in
    iter (fun elt -> acc := f !acc elt) t;
    !acc

  let fold Monoid.{ empty; append } = fold_left append empty

  let fold_right f t acc =
    let acc = ref acc in
    for i = length t - 1 downto 0 do
      acc := f (get t i) !acc
    done;
    !acc

  let to_list t =
    let len = length t in
    let rec aux i =
      if i = len then []
      else
        let x = get t i in
        x :: aux (i + 1)
    in
    aux 0

  let to_array t =
    let module A = Stdlib.Array in
    match length t with
    | 0 -> [||]
    | len ->
        let first = get t 0 in
        let res = A.make len first in
        let rec aux i =
          if i = len then ()
          else
            let x = get t i in
            A.set res i x;
            aux (i + 1)
        in
        aux 1;
        res

  let exists f t =
    let len = length t in
    let rec loop i = if i = len then false else f (get t i) || loop (i + 1) in
    loop 0

  let forall f t =
    let len = length t in
    let rec loop i = i = len || (f (get t i) && loop (i + 1)) in
    loop 0

  let mem Eq.{ equal } a t = exists (equal a) t

  (** [fold] without an initial accumulator value. Returns [None] in the case of
      an empty container. *)
  let fold_opt combine t =
    match length t with
    | 0 -> None
    | len ->
        let rec aux i acc =
          if i = len then acc
          else
            let acc = combine (get t i) acc in
            aux (i + 1) acc
        in
        Some (aux 1 (get t 0))

  let maximum ({ max; _ } : _ Ord.t) = fold_opt max

  let minimum ({ min; _ } : _ Ord.t) = fold_opt min
end
