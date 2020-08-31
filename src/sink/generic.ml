open Sink_kernel

module Pp = struct
  type 'a t = Format.formatter -> 'a -> unit [@@deriving branded]

  let empty _ = assert false
  let lazy_ elt = Fmt.using Stdlib.Lazy.force elt

  let unit, int, int32, int64, nativeint, bool, char, float, string, bytes =
    Fmt.
      ( const string "()",
        int,
        int32,
        int64,
        nativeint,
        bool,
        char,
        float,
        string,
        using Stdlib.Bytes.unsafe_to_string string )

  let option pp_elt ppf = function
    | None -> Fmt.pf ppf "None"
    | Some x -> Fmt.pf ppf "Some(%a)" pp_elt x

  let result pp_ok pp_err ppf = function
    | Ok ok -> Fmt.pf ppf "Ok(%a)" pp_ok ok
    | Error err -> Fmt.pf ppf "Error(%a)" pp_err err

  let either pp_left pp_right ppf = function
    | Left left -> Fmt.pf ppf "Left(%a)" pp_left left
    | Right right -> Fmt.pf ppf "Right(%a)" pp_right right

  let triple pp_a pp_b pp_c =
    Fmt.(
      parens
        ( using (fun (a, _, _) -> a) (box pp_a)
        ++ comma
        ++ using (fun (_, b, _) -> b) (box pp_b)
        ++ comma
        ++ using (fun (_, _, c) -> c) (box pp_c) ))

  let pair, list, array = Fmt.Dump.(pair, list, array)
end

let pp : type a. a Repr.t -> Format.formatter -> a -> unit =
  let pp = Repr.make (module Pp) in
  fun t -> Pp.prj (pp.generic t)

let to_string : type a. a Repr.t -> a -> string =
  let pp = Repr.make (module Pp) in
  fun t -> Fmt.to_to_string (Pp.prj (pp.generic t))

module Equal = struct
  type 'a t = 'a -> 'a -> bool [@@deriving branded]

  let empty _ = assert false

  let unit, int, nativeint, int32, int64, bool, char, float, string, bytes =
    (( = ), ( = ), ( = ), ( = ), ( = ), ( = ), ( = ), ( = ), ( = ), ( = ))

  let pair a b (a1, b1) (a2, b2) = a a1 a2 && b b1 b2
  let triple a b c (a1, b1, c1) (a2, b2, c2) = a a1 a2 && b b1 b2 && c c1 c2

  let option elt o1 o2 =
    match (o1, o2) with
    | None, None -> true
    | Some x, Some y -> elt x y
    | _ -> false

  let either left right e1 e2 =
    match (e1, e2) with
    | Left x, Left y -> left x y
    | Right x, Right y -> right x y
    | _ -> false

  let result ok err r1 r2 =
    match (r1, r2) with
    | Ok x, Ok y -> ok x y
    | Error x, Error y -> err x y
    | _ -> false

  let lazy_ elt l1 l2 = elt (Stdlib.Lazy.force l1) (Stdlib.Lazy.force l2)

  let rec list elt l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | x :: xs, y :: ys -> elt x y && list elt xs ys
    | _ -> false

  let array elt a1 a2 =
    let module Array = Stdlib.Array in
    let len1 = Array.length a1 and len2 = Array.length a2 in
    len1 = len2
    &&
    let rec inner i =
      i = len1 || (elt a1.(i) a2.(i) && (inner [@tailcall]) (i + 1))
    in
    inner 0
end

let equal : type a. a Repr.t -> a -> a -> bool =
  let equal = Repr.make (module Equal) in
  fun t -> Equal.prj (equal.generic t)
