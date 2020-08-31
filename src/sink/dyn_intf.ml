type dyn = Dyn : ('a Repr.t * 'a) -> dyn [@@unboxed]

module type S = sig
  type t
  type dyn

  val to_dyn : t -> dyn
end

module type S1 = sig
  type 'a t
  type dyn

  val to_dyn : ('a -> dyn) -> 'a t -> dyn
end

module type Dyn = sig
  type t [@@implements Hashable.S]

  module Encode : Repr.S with type 'a t = 'a -> t
  (* module Decode : Repr.S with type 'a t = dyn -> 'a *)

  (* module Infer : Repr.S with type 'a t = dyn -> 'a Repr.t *)

  module type S = S with type dyn := t
  module type S1 = S1 with type dyn := t
end
