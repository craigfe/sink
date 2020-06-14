open Import
module L = Stdlib.List

type 'a t = 'a list [@@deriving branded]

let t _ = failwith "TODO"

let map = L.map

let return x = [ x ]

let pure = return

let bind l f = L.map f l |> L.flatten

let kliesli f g x = bind (f x) g

let seq _ = failwith "TODO"

let lift2 _ = failwith "TODO"

let apply _ = failwith "TODO"

let null _ = failwith "TODO"

let combine _ = failwith "TODO"

(** Foldable instance *)

let fold (type m) (m : m Monoid.t) = L.fold_left m.append m.empty

let fold_left = L.fold_left

let fold_right = L.fold_right

let is_empty = function [] -> true | _ :: _ -> false

let length = L.length

let mem _ = failwith "TODO"

let maximum (type o) (o : o Ord.t) = function
  | [] -> None
  | x :: xs -> Some (L.fold_left o.max x xs)

let minimum (type o) (o : o Ord.t) = function
  | [] -> None
  | x :: xs -> Some (L.fold_left o.min x xs)

let sum = fold_left ( + ) 0

let product = fold_left ( * ) 1

let sequence_result list =
  let rec inner acc = function
    | [] -> Ok (L.rev acc)
    | Error e :: _ -> Error e
    | Ok o :: tl -> (inner [@ocaml.tailcall]) (o :: acc) tl
  in
  inner [] list

let rec unzip : type a b. (a * b) list -> a list * b list = function
  | [] -> ([], [])
  | (x, y) :: l ->
      let xs, ys = unzip l in
      (x :: xs, y :: ys)
