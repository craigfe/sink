open Import

type 'a t = 'a array
[@@implements
  Semigroup.S,
    Functor.S,
    Indexable.S,
    Foldable.S,
    Zippable.S,
    Blit.S,
    Typeable.S,
    Higher.BRANDED,
    Dyn.S]

val set : 'a t -> int -> 'a -> unit
val init : int -> (int -> 'a) -> 'a t
val to_array : 'a t -> 'a t
val of_list : 'a list -> 'a t

module Matrix : sig
  type nonrec 'a t = private 'a t t

  val make : int -> int -> 'a -> 'a t
  (** [make x_len y_len init] is a matrix of width [x] and height [y],
      containing elements that are initially physically equal to [init]. *)

  val get : 'a t -> int * int -> 'a
  (** [get m (x, y)] returns the matrix component at column [x] and row [y]. *)

  val set : 'a t -> int * int -> 'a -> unit
  (** [set m (x, y) v] sets the matrix component at column [x] and row [y] to be
      [v]. *)

  val dimensions : _ t -> int * int
  val transpose : 'a t -> 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** The pretty-printer for two-dimensional matrices *)
end
