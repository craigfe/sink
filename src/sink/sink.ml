include Toplevel
(** @inline *)

(** {1 Standard types}

    Utility modules for basic types, many of which shadow similar ones from the
    OCaml standard library. *)

module Bool = Bool
module Either = Either
module Int = Int
module Int32 = Int32
module Int64 = Int64
module Unit = Unit
module Option = Option
module Fun = Fun
module Result = Result
module Ordering = Sink_typeclasses.Ordering

(** Module types [T], [T1], [T2] ... etc. just wrap a single type [t] with an
    increasing number of type parameters. We export them into the global
    name-space. *)

module T = Sink_typeclasses.T

include T
(** @closed *)

(** {1 Containers} *)

module Seq = Seq
module List = List
module Array = Array
module Hashtbl = Hashtbl
module Set = Set

include Sink_typeclasses.Export
(** @inline *)

(** {1 Generic programming} *)

module Repr = Repr
