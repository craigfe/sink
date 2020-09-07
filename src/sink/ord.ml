include Ord_intf

let to_int_compare ord a b =
  match ord.compare a b with
  | Ordering.Lt -> -1
  | Ordering.Eq -> 0
  | Ordering.Gt -> 1

let of_int_compare compare a b =
  match compare a b with
  | n when n < 0 -> Ordering.Lt
  | n when n > 0 -> Ordering.Gt
  | _ -> Ordering.Eq

let poly (type a) (Proxy.T : a Proxy.t) : a t =
  {
    compare = of_int_compare Stdlib.compare;
    equal = Stdlib.( = );
    max = Stdlib.max;
    min = Stdlib.min;
  }

module Of_stdlib_compare (X : sig
  type t

  val compare : t -> t -> int
end) : S with type t := X.t = struct
  let equal a b = X.compare a b = 0

  let compare a b =
    match X.compare a b with
    | n when n < 0 -> Ordering.Lt
    | n when n > 0 -> Ordering.Gt
    | _ -> Ordering.Eq

  let max a b = match compare a b with Ordering.Lt -> b | Eq | Gt -> a
  let min a b = match compare a b with Ordering.Lt -> a | Eq | Gt -> b
end

module Make_infix (X : S) : INFIX with type t := X.t = struct
  open Ordering

  let ( < ) a b = match X.compare a b with Lt -> true | Eq | Gt -> false
  let ( <= ) a b = match X.compare a b with Lt | Eq -> true | Gt -> false
  let ( = ) a b = match X.compare a b with Eq -> true | Lt | Gt -> false
  let ( > ) a b = match X.compare a b with Gt -> true | Lt | Eq -> false
  let ( >= ) a b = match X.compare a b with Gt | Eq -> true | Lt -> false
  let ( <> ) a b = match X.compare a b with Lt | Gt -> true | Eq -> false
end
