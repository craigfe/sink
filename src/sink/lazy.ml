open Import

type 'a t = 'a lazy_t [@@deriving branded]

let t = Repr.lazy_

include (Stdlib.Lazy : module type of Stdlib.Lazy with type 'a t := 'a t)

let map f t = lazy (f (force t))

let return x = from_val x

let bind t f = lazy (force (f (force t)))

let join : type a. a t t -> a t = force

let kliesli f g x = bind (f x) g
