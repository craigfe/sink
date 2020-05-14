open Import

type 'a t

include Eq.S2 with type 'a t := 'a t

include Show.S2 with type 'a t := 'a t
