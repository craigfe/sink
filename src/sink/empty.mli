open! Import

type t = | [@@implements Enum.S, Typeable.S, Branded.S]

val absurd : t -> _
val all : t list
