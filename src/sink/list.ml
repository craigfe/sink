open Import
module L = Stdlib.List

type 'a t = 'a list [@@deriving branded]

let t _ = failwith "TODO"

let empty = []

let append = L.append

let equal (type e) (eq : e Eq.t) xs ys =
  let rec inner = function
    | [], [] -> true
    | x :: xs, y :: ys -> eq.equal x y && inner (xs, ys)
    | _, _ -> false
  in
  inner (xs, ys)

let map = L.map

let init = L.init

let return x = [ x ]

let bind l f = L.map f l |> L.flatten

let kliesli f g x = bind (f x) g

let join = L.flatten

let pure x = [ x ]

let apply fs xs = bind fs (fun f -> L.map (fun x -> f x) xs)

let lift2 f xs ys = bind xs (fun x -> L.map (f x) ys)

let seq _ ys = ys

let fold (type m) (m : m Monoid.t) = L.fold_left m.append m.empty

let fold_left = L.fold_left

let fold_right = L.fold_right

let null = function [] -> true | _ -> false

let length = L.length

let rec mem Eq.{ equal } a = function
  | [] -> false
  | x :: xs -> equal a x || mem { equal } a xs

let maximum (type o) ({ max; _ } : o Ord.t) = function
  | [] -> None
  | x :: xs -> Some (fold_left max x xs)

let minimum (type o) ({ min; _ } : o Ord.t) = function
  | [] -> None
  | x :: xs -> Some (fold_left min x xs)

let iter = L.iter

let to_list t = t

let to_array = Stdlib.Array.of_list

let product = fold_left ( * ) 1

let sum = fold_left ( + ) 0

let is_empty = function [] -> true | _ :: _ -> false

let sequence_result list =
  let rec inner acc = function
    | [] -> Ok (L.rev acc)
    | Error e :: _ -> Error e
    | Ok o :: tl -> (inner [@ocaml.tailcall]) (o :: acc) tl
  in
  inner [] list

let rec zip xs ys =
  match (xs, ys) with x :: xs, y :: ys -> (x, y) :: zip xs ys | _, _ -> []

let rec zip_with f xs ys =
  match (xs, ys) with
  | x :: xs, y :: ys -> f x y :: zip_with f xs ys
  | _, _ -> []

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: l ->
      let xs, ys = unzip l in
      (x :: xs, y :: ys)

let rec unzip_with f = function
  | [] -> ([], [])
  | hd :: tl ->
      let x, y = f hd in
      let xs, ys = unzip_with f tl in
      (x :: xs, y :: ys)

let rev = L.rev

let take_while p =
  let rec aux acc = function
    | [] -> rev acc
    | x :: xs when p x -> (aux [@tailcall]) (x :: acc) xs
    | _ :: xs -> (aux [@tailcall]) acc xs
  in
  aux []

let rec drop_while p = function x :: xs when p x -> drop_while p xs | l -> l
