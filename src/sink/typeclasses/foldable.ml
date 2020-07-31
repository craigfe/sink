open Import
include Foldable_intf

module Of_left_assoc (X : Left_assoc) :
  General_indexed
    with type ('a, 'p) t := ('a, 'p) X.t
     and type 'a elt := 'a X.elt
     and type index := X.index = struct
  include X

  let reduce (type m) (m : m elt Monoid.t) = fold_left m.append m.empty

  let is_empty l =
    fold_until (fun () _elt -> Right ()) () l |> function
    | Left () -> true
    | Right () -> false

  let mem (type a) ({ equal } : a elt Eq.t) elt =
    fold_left (fun a x -> a || equal x elt) false

  let iter f = fold_left (fun () -> f) ()
  let iteri f = fold_lefti (fun i () -> f i) ()

  let maximum (type o) ({ max; _ } : o elt Ord.t) t =
    match decons t with
    | None -> None
    | Some (x, xs) -> Some (fold_left max x xs)

  let minimum (type o) ({ min; _ } : o elt Ord.t) t =
    match decons t with
    | None -> None
    | Some (x, xs) -> Some (fold_left min x xs)

  let fold_right _ = failwith "TODO"
  let length t = fold_left (fun acc _ -> acc + 1) 0 t

  let for_alli p t =
    fold_untili
      (fun i () elt -> match p i elt with true -> Left () | false -> Right ())
      () t
    |> function
    | Left () -> true
    | Right () -> false

  let for_all p = for_alli (fun _ -> p)

  let existsi p t =
    fold_untili
      (fun i () elt -> match p i elt with false -> Left () | true -> Right ())
      () t
    |> function
    | Left () -> false
    | Right () -> true

  let exists p = existsi (fun _ -> p)
  let to_list t = fold_left (fun acc e -> e :: acc) [] t |> List.rev
end

module Of_indexable (X : Indexable.General) = struct
  open X

  let length = length
  let is_empty t = length t = 0

  let iteri f t =
    for i = 0 to length t - 1 do
      f i (get t i)
    done

  let iter f = iteri (fun _ -> f)

  let fold_lefti f acc t =
    let acc = ref acc in
    iteri (fun i elt -> acc := f i !acc elt) t;
    !acc

  let fold_left f = fold_lefti (fun _ -> f)

  let fold_untili f acc t =
    let len = length t in
    let acc = ref acc in
    let rec loop i =
      if i = len then Left !acc
      else
        match f i !acc (get t i) with
        | Left acc' ->
            acc := acc';
            loop (i + 1)
        | Right _ as r -> r
    in
    loop 0

  let fold_until f = fold_untili (fun _ -> f)
  let reduce Monoid.{ empty; append } = fold_left append empty

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

  (* let to_array t =
     *   let module A = Stdlib.Array in
     *   match length t with
     *   | 0 -> [||]
     *   | len ->
     *       let first = get t 0 in
     *       let res = A.make len first in
     *       let rec aux i =
     *         if i = len then ()
     *         else
     *           let x = get t i in
     *           A.set res i x;
     *           aux (i + 1)
     *       in
     *       aux 1;
     *       res *)

  let existsi f t =
    let len = length t in
    let rec loop i = if i = len then false else f i (get t i) || loop (i + 1) in
    loop 0

  let exists f = existsi (fun _ -> f)

  let for_alli f t =
    let len = length t in
    let rec loop i = i = len || (f i (get t i) && loop (i + 1)) in
    loop 0

  let for_all f = for_alli (fun _ -> f)
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
