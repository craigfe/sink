module Export = struct
  (** {1 Typeclasses for data} *)

  module Semigroup = Semigroup
  (** Types with an associative binary operation {{!Semigroup.S.combine}
      [combine]}. *)

  module Monoid = Monoid
  (** {!Semigroup}s extended with an {{!Monoid.S.empty} [empty]} identity
      element that is an identity with respect to {{!Semigroup.S.combine}
      [combine]}. *)

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
end

include Export
module Ordering = Ordering
