open! Import
open Dyn_intf

type t = dyn

module type S = S with type dyn := t
module type S1 = S1 with type dyn := t

module Encode : Repr.S with type 'a t = 'a -> t = struct
  type nonrec 'a t = 'a -> t [@@deriving branded]

  let v : type a. a Repr.t -> a t = fun t x -> Dyn (t, x)
  let empty = absurd
  let unit = v Repr.unit
  let bool = v Repr.bool
  let char = v Repr.char
  let string = v Repr.string
  let bytes = v Repr.bytes
  let int = v Repr.int
  let nativeint = v Repr.nativeint
  let int32 = v Repr.int32
  let int64 = v Repr.int64
  let float = v Repr.float

  let list _enc_elt = function
    | [] -> Dyn (Repr.(list empty), [])
    | _ :: _ -> failwith "TODO"

  let array _ = failwith "TODO"

  let option enc_a = function
    | None -> Dyn (Repr.(option empty), None)
    | Some a -> (
        match enc_a a with Dyn (repr_a, a) -> Dyn (Repr.option repr_a, Some a) )

  let either enc_l enc_r = function
    | Left l -> (
        match enc_l l with
        | Dyn (repr_l, l) -> Dyn (Repr.(either repr_l empty), Left l) )
    | Right r -> (
        match enc_r r with
        | Dyn (repr_r, r) -> Dyn (Repr.(either empty repr_r), Right r) )

  let result enc_ok enc_err = function
    | Ok o -> (
        match enc_ok o with
        | Dyn (repr_o, o) -> Dyn (Repr.(result repr_o empty), Ok o) )
    | Error e -> (
        match enc_err e with
        | Dyn (repr_e, e) -> Dyn (Repr.(result empty repr_e), Error e) )

  let lazy_ enc v =
    match enc (Stdlib.Lazy.force v) with
    | Dyn (repr_v, v) -> Dyn (Repr.(lazy_ repr_v), lazy v)

  let pair enc_a enc_b (a, b) =
    match enc_a a with
    | Dyn (repr_a, a) -> (
        match enc_b b with
        | Dyn (repr_b, b) -> Dyn (Repr.pair repr_a repr_b, (a, b)) )

  let triple enc_a enc_b enc_c (a, b, c) =
    match enc_a a with
    | Dyn (repr_a, a) -> (
        match enc_b b with
        | Dyn (repr_b, b) -> (
            match enc_c c with
            | Dyn (repr_c, c) ->
                Dyn (Repr.triple repr_a repr_b repr_c, (a, b, c)) ) )
end

let hash = Stdlib.Hashtbl.hash
