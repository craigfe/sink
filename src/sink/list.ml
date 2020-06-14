open Import
module L = Stdlib.List

type 'a t = 'a list [@@deriving branded]

let t _ = failwith "TODO"

include (
  struct
    let combine _ = failwith "TODO"
  end :
    Semigroup.S1 with type 'a t := 'a t )

include (
  struct
    let equal (type e) (eq : e Eq.t) xs ys =
      let rec inner = function
        | [], [] -> true
        | x :: xs, y :: ys -> eq.equal x y && inner (xs, ys)
        | _, _ -> false
      in
      inner (xs, ys)
  end :
    Eq.S1 with type 'a t := 'a t )

include (
  struct
    let map = L.map
  end :
    Functor.S1 with type 'a t := 'a t )

include (
  struct
    let return x = [ x ]

    let bind l f = L.map f l |> L.flatten

    let kliesli f g x = bind (f x) g
  end :
    Monad.S1 with type 'a t := 'a t )

include (
  struct
    let pure x = [ x ]

    let apply fs xs = bind fs (fun f -> L.map (fun x -> f x) xs)

    let lift2 f xs ys = bind xs (fun x -> L.map (f x) ys)

    let seq _ ys = ys
  end :
    Applicative.S1 with type 'a t := 'a t )

include (
  struct
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

    let product = fold_left ( * ) 1

    let sum = fold_left ( + ) 0
  end :
    Foldable.S1 with type 'a t := 'a t )

let is_empty = function [] -> true | _ :: _ -> false

let length = L.length

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
