open Sink_kernel
module L = Stdlib.List

module T = struct
  type 'a t = 'a list [@@deriving branded]
  type 'a elt = 'a
  type index = int

  let hd_exn = L.hd
  let nth = L.nth_opt
  let nth_exn = L.nth
  let t = Repr.list
  let to_dyn = Dyn.Encoder.list
  let empty = []
  let append a b = match (a, b) with x, [] | [], x -> x | a, b -> L.append a b
  let decons = function [] -> None | x :: xs -> Some (x, xs)

  let equal (type e) (eq : e Eq.t) xs ys =
    let rec inner = function
      | [], [] -> true
      | x :: xs, y :: ys -> eq.equal x y && inner (xs, ys)
      | _, _ -> false
    in
    inner (xs, ys)

  let map = L.map
  let mapi = L.mapi
  let init = L.init
  let return x = [ x ]
  let bind l f = L.map f l |> L.flatten
  let flat_map f t = bind t f
  let kliesli f g x = bind (f x) g
  let join = L.flatten
  let pure x = [ x ]
  let apply fs xs = bind fs (fun f -> L.map (fun x -> f x) xs)
  let lift2 f xs ys = bind xs (fun x -> L.map (f x) ys)
  let seq _ ys = ys
  let fold_left = L.fold_left

  let fold_lefti f =
    let rec aux i acc = function
      | [] -> acc
      | x :: xs -> aux (i + 1) (f i acc x) xs
    in
    aux 0

  let rec fold_until f acc = function
    | [] -> Left acc
    | x :: xs -> (
        match f acc x with Left acc -> fold_until f acc xs | Right _ as r -> r )

  let fold_untili f =
    let rec aux i acc = function
      | [] -> Left acc
      | x :: xs -> (
          match f i acc x with
          | Left acc -> aux (i + 1) acc xs
          | Right _ as r -> r )
    in
    aux 0

  let fold_right = L.fold_right
  let length = L.length

  let rec mem Eq.{ equal } a = function
    | [] -> false
    | x :: xs -> equal a x || mem { equal } a xs

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

  let distrib_result l =
    fold_right
      (fun a b ->
        match (a, b) with
        | Ok o, Ok acc -> Ok (o :: acc)
        | Ok _, Error e -> Error e
        | Error e, Error acc -> Error (e :: acc)
        | Error e, Ok _ -> Error [ e ])
      l (Ok [])

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

  let rec partition_map f = function
    | [] -> ([], [])
    | x :: xs -> (
        let ls, rs = partition_map f xs in
        match f x with Left l -> (l :: ls, rs) | Right r -> (ls, r :: rs) )

  let partition p = partition_map (fun x -> if p x then Left x else Right x)
  let rev = L.rev
  let rev_append = L.rev_append

  let take_while p =
    let rec aux acc = function
      | [] -> rev acc
      | x :: xs when p x -> (aux [@tailcall]) (x :: acc) xs
      | _ :: xs -> (aux [@tailcall]) acc xs
    in
    aux []

  let rec drop_while p = function x :: xs when p x -> drop_while p xs | l -> l

  let filteri p =
    let rec aux i = function
      | [] -> []
      | x :: xs -> (
          match p i x with
          | true -> x :: aux (i + 1) xs
          | false -> aux (i + 1) xs )
    in
    aux 0

  let filter f = filteri (fun _ -> f)

  let filter_mapi f =
    let rec aux i = function
      | [] -> []
      | x :: xs -> (
          match f i x with
          | Some y -> y :: aux (i + 1) xs
          | None -> aux (i + 1) xs )
    in
    aux 0

  let filter_map f = filter_mapi (fun _ -> f)

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list

    let inverse t = map (fun (a, b) -> (b, a)) t
  end
end

include Foldable.Of_left_assoc (struct
  include T

  type nonrec ('a, 'p) t = 'a T.t
end)

include T

let of_array = Stdlib.Array.to_list
