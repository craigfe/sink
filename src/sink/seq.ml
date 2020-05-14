type nonrec +'a node = 'a Stdlib.Seq.node = Nil | Cons of 'a * 'a Stdlib.Seq.t

type nonrec +'a t = 'a Stdlib.Seq.t

let empty () = Nil

let return x () = Cons (x, empty)

let cons x next () = Cons (x, next)

let rec append seq1 seq2 () =
  match seq1 () with
  | Nil -> seq2 ()
  | Cons (x, next) -> Cons (x, append next seq2)

let rec map f seq () =
  match seq () with Nil -> Nil | Cons (x, next) -> Cons (f x, map f next)

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

let rec flat_map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> flat_map_app f (f x) next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () =
  match seq () with
  | Nil -> flat_map f tail ()
  | Cons (x, next) -> Cons (x, flat_map_app f next tail)

let fold_left f acc seq =
  let rec aux f acc seq =
    match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  aux f acc seq

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
