type t
(** The type of file paths. *)

val v : string -> t
(** Interpret a string as a path.

    @raise Invalid_argument if the argument is not a valid path. *)
