module Pp = struct
  type 'a t = Format.formatter -> 'a -> unit [@@deriving branded]

  let empty _ = assert false
  let lazy_ elt = Fmt.using Stdlib.Lazy.force elt

  let unit, int, int32, bool, char, float, string, bytes =
    Fmt.
      ( const string "()",
        int,
        int32,
        bool,
        char,
        float,
        string,
        using Stdlib.Bytes.unsafe_to_string string )

  let option pp_elt ppf = function
    | None -> Fmt.pf ppf "None"
    | Some x -> Fmt.pf ppf "Some(%a)" pp_elt x

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

  let unit, int, int32, bool, char, float, string, bytes =
    (( = ), ( = ), ( = ), ( = ), ( = ), ( = ), ( = ), ( = ))

  let pair a b (a1, b1) (a2, b2) = a a1 a2 && b b1 b2

  let option elt o1 o2 =
    match (o1, o2) with
    | None, None -> true
    | Some x, Some y -> elt x y
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
