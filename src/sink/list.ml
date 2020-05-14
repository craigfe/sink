module L = Stdlib.List
open Import

module T = Higher.Newtype1 (struct
  type nonrec 'a t = 'a list
end)

include T

let t _ = failwith "TODO"

(** Foldable instance *)

(* let fold (type m) (m : m Monoid.t) = L.fold_left m.append m.empty
 * 
 * let fold_left = L.fold_left
 * 
 * let fold_right = L.fold_right
 * 
 * let empty = function [] -> true | _ :: _ -> false
 * 
 * let length = L.length
 * 
 * let mem = L.mem *)

(* let maximum (type o) (o : o Ord.t) = function
 *   | [] -> None
 *   | x :: xs -> Some (L.fold_left o.max x xs)
 * 
 * let minimum (type o) (o : o Ord.t) = function
 *   | [] -> None
 *   | x :: xs -> Some (L.fold_left o.min x xs) *)

(* let sum = fold_left ( + ) 0
 * 
 * let product = fold_left ( * ) 1 *)
