module type S = sig
  type t

  val empty : t

  val append : t -> t -> t
end

type 't t = { empty : 't; append : 't -> 't -> 't }
