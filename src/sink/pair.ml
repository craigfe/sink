type ('a, 'b) t = 'a * 'b

let v a b = (a, b)
let flip (a, b) = (b, a)
let fst (a, _) = a
let snd (_, b) = b
let map f (a, b) = (f a, f b)
let map1 f (a, b) = (f a, b)
let map2 f (a, b) = (a, f b)

module Fst = struct end
