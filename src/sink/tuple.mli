open Import

module T2 : sig
  type ('a, 'b) t = 'a * 'b [@@implements Eq.S]

  (** Functor instances on each of the components: *)

  val first : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t

  val second : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c [@@implements Eq.S]

  (** Functor instances on each of the components: *)

  val first : ('a1 -> 'a2) -> ('a1, 'b, 'c) t -> ('a2, 'b, 'c) t

  val second : ('b1 -> 'b2) -> ('a, 'b1, 'c) t -> ('a, 'b2, 'c) t

  val third : ('c1 -> 'c2) -> ('a, 'b, 'c1) t -> ('a, 'b, 'c2) t
end

module T4 : sig
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd [@@implements Eq.S]

  (** Functor instances on each of the components: *)

  val first : ('a1 -> 'a2) -> ('a1, 'b, 'c, 'd) t -> ('a2, 'b, 'c, 'd) t

  val second : ('b1 -> 'b2) -> ('a, 'b1, 'c, 'd) t -> ('a, 'b2, 'c, 'd) t

  val third : ('c1 -> 'c2) -> ('a, 'b, 'c1, 'd) t -> ('a, 'b, 'c2, 'd) t

  val fourth : ('d1 -> 'd2) -> ('a, 'b, 'c, 'd1) t -> ('a, 'b, 'c, 'd2) t
end
