module type Generic = sig
  type ('a, 'p) t
  type 'a elt

  val map : ('a elt -> 'b elt) -> ('a, 'p) t -> ('b, 'p) t
end

module type Generic_indexed = sig
  type ('a, 'p) t
  type 'a elt
  type index

  include Generic with type ('a, 'p) t := ('a, 'p) t and type 'a elt := 'a elt

  val mapi : (index -> 'a elt -> 'b elt) -> ('a, 'p) t -> ('b, 'p) t
end

module type S = sig
  type t
  type elt

  (** @inline *)
  include Generic with type (_, _) t := t and type _ elt := elt
end

module type Indexed = sig
  type t
  type elt
  type index

  (** @inline *)
  include
    Generic_indexed
      with type (_, _) t := t
       and type _ elt := elt
       and type index := index
end

module type S1 = sig
  type 'a t

  (** @inline *)
  include Generic with type ('a, _) t := 'a t and type 'a elt := 'a
end

module type Indexed1 = sig
  type 'a t
  type index

  (** @inline *)
  include
    Generic_indexed
      with type ('a, _) t := 'a t
       and type 'a elt := 'a
       and type index := index
end

module type S2 = sig
  type ('a, 'p) t

  (** @inline *)
  include Generic with type ('a, 'p) t := ('a, 'p) t and type 'a elt := 'a
end

module type INFIX = sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix alias of {!S.map}. *)
end

module type INFIX2 = sig
  type ('a, 'e) t

  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  (** Infix alias of {!S.map}. *)
end

module type SYNTAX = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Syntax alias of {!S.map}. *)
end

module type EXT = sig
  include S1
  module Infix : INFIX with type 'a t := 'a t
  module Syntax : SYNTAX with type 'a t := 'a t
end
