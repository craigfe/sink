type 'a t = 'a list

(** Typeclass instances: *)

(* include Semigroup.S1 with type 'a t := 'a t *)
(** @closed *)

(* include Foldable.S with type 'a t := 'a t *)
(** @closed *)

(* include Functor.S with type 'a t := 'a t *)
(** @closed *)

(* include Applicative.S with type 'a t := 'a t *)
(** @closed *)

(* include Monad.S with type 'a t := 'a t *)
(** @closed *)

include Typeable.S1 with type 'a t := 'a t
(** @closed *)

(* module Infix : sig
 *   include Semigroup.INFIX1 with type 'a t := 'a t
 *   (\** @closed *\)
 * 
 *   include Functor.INFIX with type 'a t := 'a t
 *   (\** @closed *\)
 * 
 *   include Applicative.INFIX with type 'a t := 'a t
 *   (\** @closed *\)
 * 
 *   include Monad.INFIX with type 'a t := 'a t
 *   (\** @closed *\)
 * end *)
