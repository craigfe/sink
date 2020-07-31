include Toplevel
(** @inline *)

(** {1 Standard types}

    Utility modules for basic types, many of which shadow similar ones from the
    OCaml standard library. *)

module Unit = Unit
module Bool = Bool
module Int = Int
module Int32 = Int32
module Int64 = Int64
module Fun = Fun
module Either = Either
module Option = Option
module Result = Result
module Ordering = Sink_typeclasses.Ordering
module Empty = Empty

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
module Set = Set

(** {2 Association containers} *)

module Assoc = Sink_typeclasses.Assoc
module Hashtbl = Hashtbl
module Map = Map

include Sink_typeclasses.Export
(** @inline *)

(** {1 Generic programming} *)

module Repr = Repr
