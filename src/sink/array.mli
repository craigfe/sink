open Import

type 'a t = 'a array [@@implements Higher.BRANDED]

(* include Foldable.S with type 'a t := 'a t
 * 
 * include Functor.S with type 'a t := 'a t
 * 
 * include Typeable.S1 with type 'a t := 'a t
 * 
 * module Infix : sig
 *   include Functor.INFIX with type 'a t := 'a t
 * end *)
