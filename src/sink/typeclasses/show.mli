type 't t = private { show : 't -> string }

module type S = sig
  type t

  val to_string : t -> string
end

module type S2 = sig
  type 'a t

  val to_string : 'a t -> string
end

module type INFIX = sig
  type t
end
