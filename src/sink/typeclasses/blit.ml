type ('src, 'dst) blit =
  src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit

module type S = sig
  type t

  val blit : (t, t) blit
end
[@@deriving phantom]
