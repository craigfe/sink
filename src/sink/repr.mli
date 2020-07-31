open Import

type 'a t [@@implements Eq.S, Show.S, Higher.BRANDED]

(* Implements [Typeable.S1] (which depends on this module, so cannot be included here) *)
val t : 'a t -> 'a t t
val empty : empty t
val unit : unit t
val int : int t
val int32 : int32 t
val bool : bool t
val char : char t
val string : string t
val bytes : bytes t
val float : float t
val lazy_ : 'a t -> 'a Stdlib.Lazy.t t
val option : 'a t -> 'a option t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val pair : 'a t -> 'b t -> ('a * 'b) t
