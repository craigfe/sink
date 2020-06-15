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

let fold_left f acc seq =
  let rec aux f acc seq =
    match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  aux f acc seq

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

let rec to_list seq =
  match seq () with Nil -> [] | Cons (x, xf) -> x :: to_list xf

let to_array seq = to_list seq |> Stdlib.Array.of_list

let is_empty seq = match seq () with Nil -> true | Cons (_, _) -> false
