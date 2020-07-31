module type Generic = sig
  type ('k, 'v) t
  type 'k key
  type 'v value

  val mem : ('k, _) t -> 'k key -> bool
  val find : ('k, _) t -> 'k key

  include Functor.S2 with type ('v, 'k) t := ('k, 'v) t
  (** @closed *)

  module Key : sig
    include Functor.S2 with type ('k, 'v) t := ('k, 'v) t
    (** @closed *)
  end
end

module type Generic_functional = sig
  type ('k, 'v) t
  type 'k key
  type 'v value

  val empty : ('k, 'v) t

  (** @inline *)
  include
    Generic
      with type ('k, 'v) t := ('k, 'v) t
       and type 'k key := 'k key
       and type 'v value := 'v value

  val add : 'k key -> 'v value -> ('k, 'v) t -> ('k, 'v) t
  val update : 'k key -> ('v option -> 'v option) -> ('k, 'v) t -> ('k, 'v) t
end

module type F2 = sig
  (** @inline *)
  include Generic with type 'k key := 'k and type 'v value := 'v
end

module type Generic_mutable = sig
  type ('k, 'v) t
  type 'k key
  type 'v value

  val create : unit -> ('k, 'v) t

  (** @inline *)
  include
    Generic
      with type ('k, 'v) t := ('k, 'v) t
       and type 'k key := 'k key
       and type 'v value := 'v value

  val clear : (_, _) t -> unit
end
