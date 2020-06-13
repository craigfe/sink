open Import

type t = bool [@@implements Eq.S, Show.S, Ord.S, Higher.BRANDED]

external not : bool -> bool = "%boolnot"

external ( && ) : bool -> bool -> bool = "%sequand"

external ( || ) : bool -> bool -> bool = "%sequor"

external to_int : bool -> int = "%identity"

val to_float : bool -> float
