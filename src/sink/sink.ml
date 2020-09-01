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
module Ordering = Ordering
module Empty = Empty
module Char = Char
module String = String
module Bytes = Bytes
module Tuple = Tuple
module Pair = Tuple.T2
module Triple = Tuple.T3

(** Module types [T], [T1], [T2] ... etc. just wrap a single type [t] with an
    increasing number of type parameters. We export them into the global
    name-space. *)

module T = T

include T
(** @closed *)

(** {1 Containers} *)

module Seq = Seq
module List = List
module Array = Array
module Set = Set

(** {2 Association containers} *)

module Assoc = Assoc
module Hashtbl = Hashtbl
module Map = Map

(** {1 Typeclasses for data} *)

module Semigroup = Semigroup
(** Types with an associative binary operation {{!Semigroup.S.combine}
    [combine]}. *)

module Monoid = Monoid
(** {!Semigroup}s extended with an {{!Monoid.S.empty} [empty]} identity element
    that is an identity with respect to {{!Semigroup.S.combine} [combine]}. *)

module Eq = Eq
(** Types with an equality operation. *)

module Ord = Ord
(** Types with a partial order. *)

module Bounded = Bounded
(** Types with a minimum and maximum value. *)

module Enum = Enum

module Num = Num
(** Numeric types: supporting addition, subtraction, negation and
    multiplication. *)

module Show = Show

(** {1 Typeclasses for containers / effects} *)

module Foldable = Foldable
(** Containers that support [fold] operations. *)

module Filterable = Filterable
(** Containers that support [filter] operations. *)

module Zippable = Zippable
(** Containers that can be [zip]ed and [unzip]ed. *)

module Indexable = Indexable
module Hashable = Hashable

module Blit = Blit
(** Containers with a contiguous memory representation. *)

module Functor = Functor
module Applicative = Applicative
module Monad = Monad
module Proxy = Proxy

(** {1 Higher-kinded polymorphism} *)

module Higher = Higher
module Reifier = Reifier

(** {1 Generic programming} *)

module Repr = Repr
module Generic = Generic
