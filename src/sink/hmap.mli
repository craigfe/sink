type t

module Key : sig
  type 'a t

  val v : string -> ('a -> Dyn.t) -> 'a t
end

val empty : t
val is_empty : t -> bool
val mem : t -> 'a Key.t -> bool
val add : t -> 'a Key.t -> 'a -> t
val remove : t -> 'a Key.t -> t
val find : t -> 'a Key.t -> 'a option
val find_exn : t -> 'a Key.t -> 'a
val singleton : 'a Key.t -> 'a -> t
val to_dyn : t -> Dyn.t
