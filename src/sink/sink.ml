open Import

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
module Ordering = Ordering

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
module Hashtbl = Hashtbl
module Set = Set

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

module Num = Num
(** Numeric types: supporting addition, subtraction, negation and
    multiplication. *)

module Foldable = Foldable
(** Containers that support [fold] operations. *)

module Show = Show

(** {1 Typeclasses for containers / effects} *)

module Functor = Functor
module Applicative = Applicative
module Monad = Monad

(** {1 Higher-kinded polymorphism} *)

module Higher = Higher

(** {1 Generic programming} *)

module Repr = Repr
