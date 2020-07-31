val mkdir_p : string -> unit
val print_to_file : string -> (Format.formatter -> unit) -> unit
val sequence_commands : string list -> (unit, [ `Msg of string ]) result
