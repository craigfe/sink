module type S = sig
  type t

  val combine : t -> t -> t
end
[@@deriving typeclass]

module type INFIX = sig
  type t

  val ( <> ) : t -> t -> t
end

module type S1 = sig
  type 'a t

  val combine : 'a t -> 'a t -> 'a t
end

module type INFIX1 = sig
  type 'a t

  val ( <> ) : 'a t -> 'a t -> 'a t
end
