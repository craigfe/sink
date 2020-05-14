module type MINIMAL = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module type S = sig
  type 'a t

  val fold : 'm Monoid.t -> 'm t -> 'm

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

  val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val null : 'a t -> bool

  val length : 'a t -> int

  val mem : 'a Eq.t -> 'a -> 'a t -> bool

  val maximum : 'a Ord.t -> 'a t -> 'a option

  val minimum : 'a Ord.t -> 'a t -> 'a option

  val sum : int t -> int

  val product : int t -> int
end
[@@deriving typeclass]

module type Foldable = sig
  module type S = S

  module Make_default (X : MINIMAL) : S with type 'a t := 'a X.t

  (* module Dict : sig
   *   type 't fold_left =
   *     < fold_left :
   *         'a 'acc. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 't) app -> 'acc >
   * 
   *   type 't fold_right =
   *     < fold_right :
   *         'a 'acc. ('a -> 'acc -> 'acc) -> ('a, 't) app -> 'acc -> 'acc >
   * 
   *   type 't fold = < fold : 'm. 'm Monoid.t -> ('m, 't) app -> 'm >
   * 
   *   type 't null = < null : 'a. ('a, 't) app -> bool >
   * 
   *   val create :
   *     fold_left:< 't fold_left ; .. > ->
   *     fold_right:< 't fold_right ; .. > ->
   *     (\* Derivable implementations *\)
   *     ?fold:< 't fold ; .. > ->
   *     ?null:< 't null ; .. > ->
   *     ?length:< length : 'a. ('a, 't) app -> int ; .. > ->
   *     ?mem:< mem : 'a. 'a Eq.t -> 'a -> ('a, 't) app -> bool ; .. > ->
   *     ?maximum:< maximum : 'a. 'a Ord.t -> ('a, 't) app -> 'a ; .. > ->
   *     ?minimum:< minimum : 'a. 'a Ord.t -> ('a, 't) app -> 'a ; .. > ->
   *     ?sum:< sum : (int, 't) app -> int ; .. > ->
   *     ?product:< product : (int, 't) app -> int ; .. > ->
   *     unit ->
   *     't t
   * end *)
end
