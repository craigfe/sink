open Import

type t = bool

external not : bool -> bool = "%boolnot"

external ( && ) : bool -> bool -> bool = "%sequand"

external ( || ) : bool -> bool -> bool = "%sequor"

external to_int : bool -> int = "%identity"

val to_float : bool -> float

(** Typeclass instances: *)

include Eq.S with type t := t
(** @closed *)

include Show.S with type t := t
(** @closed *)

include Ord.S with type t := t
(** @closed *)
