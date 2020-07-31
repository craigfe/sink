type t = { start : Lexing.position; stop : Lexing.position }

let equal_position (a : Lexing.position) (b : Lexing.position) =
  a.pos_fname = b.pos_fname
  && a.pos_lnum = b.pos_lnum
  && a.pos_bol = b.pos_bol
  && a.pos_cnum = b.pos_cnum

let equal a b = equal_position a.start b.start && equal_position a.stop b.stop
let to_dyn _ = failwith "TODO"
