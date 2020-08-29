open Import

type 'a t = 'a option
[@@implements Eq.S, Ord.S, Functor.S, Applicative.S, Monad.S, Higher.BRANDED]

val none : _ t

val some : 'a -> 'a t
(** See also {!pure}. *)

val get : 'a t -> 'a
(** Get the value [v] from a [Some v].

    @raise Invalid_argument if the argument is [None]. *)

val to_result : none:'b -> 'a t -> ('a, 'b) Result.t
val to_list : 'a t -> 'a List.t
val to_seq : 'a t -> 'a Seq.t
