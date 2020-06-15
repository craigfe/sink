open Import

type ('a, 'b) t = 'a -> 'b [@@deriving branded]

external id : 'a -> 'a = "%identity"

let const c _ = c

let flip f x y = f y x

let negate p v = not (p v)

let uncurry f (x, y) = f x y

let curry f x y = f (x, y)
