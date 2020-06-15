open Import

type _ t =
  | Unit : unit t
  | Int : int t
  | Int32 : int32 t
  | Bool : bool t
  | Char : char t
  | String : string t
  | Bytes : bytes t
  | Float : float t
  | Option : 'a t -> 'a option t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Lazy : 'a t -> 'a Stdlib.Lazy.t t
  | Pair : ('a t * 'b t) -> ('a * 'b) t
  | Repr : 'a t -> 'a t t
[@@deriving branded]

let t t = Repr t

let unit = Unit

let int = Int

let int32 = Int32

let bool = Bool

let char = Char

let string = String

let bytes = Bytes

let float = Float

let option a = Option a

let list a = List a

let array a = Array a

let lazy_ a = Lazy a

let pair a b = Pair (a, b)

let to_string = assert false

let equal = assert false
