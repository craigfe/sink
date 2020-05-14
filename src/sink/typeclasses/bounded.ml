module type S = sig
  type t

  val minimum : t

  val maximum : t
end
[@@deriving typeclass]
