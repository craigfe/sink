open Import

type nonrec +'a node = 'a Stdlib.Seq.node = Nil | Cons of 'a * 'a Stdlib.Seq.t
type nonrec +'a t = 'a Stdlib.Seq.t [@@deriving branded]

let cons x next () = Cons (x, next)
let t _ = failwith "TODO"
let empty () = Nil

let rec append seq1 seq2 () =
  match seq1 () with
  | Nil -> seq2 ()
  | Cons (x, next) -> Cons (x, append next seq2)

let equal (type e) (eq : e Eq.t) xs ys =
  let rec inner xs ys =
    match (xs (), ys ()) with
    | Nil, Nil -> true
    | Cons (x, xf), Cons (y, yf) -> eq.equal x y && inner xf yf
    | _, _ -> false
  in
  inner xs ys

let rec map f xs () =
  match xs () with Nil -> Nil | Cons (x, xf) -> Cons (f x, map f xf)

module Monad_instance = struct
  type nonrec 'a t = 'a t

  let return x () = Cons (x, empty)

  let rec bind seq f () =
    match seq () with
    | Nil -> Nil
    | Cons (x, next) -> flat_map_app f (f x) next ()

  (* this is [append seq (flat_map f tail)] *)
  and flat_map_app f seq tail () =
    match seq () with
    | Nil -> bind tail f ()
    | Cons (x, next) -> Cons (x, flat_map_app f next tail)

  let kliesli f g x = bind (f x) g
  let join t = Stdlib.Seq.flat_map Fun.id t
end

include (Monad_instance : Monad.S1 with type 'a t := 'a t)
include Applicative.Of_monad (Monad_instance)

module Left_assoc = struct
  type nonrec ('a, 'p) t = 'a t
  type 'a elt = 'a
  type index = int

  let fold_left f =
    let rec aux acc seq =
      match seq () with
      | Nil -> acc
      | Cons (x, next) ->
          let acc = f acc x in
          aux acc next
    in
    aux

  let fold_lefti f =
    let rec aux i acc seq =
      match seq () with
      | Nil -> acc
      | Cons (x, next) ->
          let acc = f i acc x in
          aux (i + 1) acc next
    in
    aux 0

  let fold_until f =
    let rec aux acc t =
      match t () with
      | Nil -> Left acc
      | Cons (x, next) -> (
          match f acc x with Left acc -> aux acc next | Right _ as r -> r )
    in
    aux

  let fold_untili f =
    let rec aux i acc t =
      match t () with
      | Nil -> Left acc
      | Cons (x, next) -> (
          match f i acc x with
          | Left acc -> aux (i + 1) acc next
          | Right _ as r -> r )
    in
    aux 0

  let decons seq =
    match seq () with Nil -> None | Cons (x, next) -> Some (x, next)
end

include Foldable.Of_left_assoc (Left_assoc)

let rec filter_map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> (
      match f x with
      | None -> filter_map f next ()
      | Some y -> Cons (y, filter_map f next) )

let rec filter f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> if f x then Cons (x, filter f next) else filter f next ()

let iter f seq =
  let rec aux seq =
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f x;
        aux next
  in
  aux seq

let rec unfold f u () =
  match f u with None -> Nil | Some (x, u') -> Cons (x, unfold f u')
