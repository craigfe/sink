module type S = sig
  type t

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val max : t -> t -> t

  val min : t -> t -> t
end
[@@deriving typeclass]

module type S1 = sig
  type 'a t

  val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val max : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val min : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module type INFIX = sig
  type t

  val ( < ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( = ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( <> ) : t -> t -> bool
end

module type Ord = sig
  type nonrec 't t = 't t

  module type S = S

  module type S1 = S1

  module type INFIX = INFIX

  val poly : 'a Proxy.t -> 'a t

  module Of_stdlib_compare (X : sig
    type t

    val compare : t -> t -> int
  end) : S with type t := X.t

  module Make_infix (X : S) : INFIX with type t := X.t
end
