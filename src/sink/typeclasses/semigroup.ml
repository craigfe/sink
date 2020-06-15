(* TODO: remove? *)
module type S = sig
  type t

  val combine : t -> t -> t [@@infix ( <> )]
end
[@@deriving typeclass, infix, phantom { subderiving = infix }]
