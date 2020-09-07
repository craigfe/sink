open! Import

module type Minimal = sig
  type (+'a, 'p) t

  val return : 'a -> ('a, 'p) t
  val bind : ('a, 'p) t -> ('a -> ('b, 'p) t) -> ('b, 'p) t
end

module type Generic = sig
  type (+'a, 'p) t

  val return : 'a -> ('a, 'p) t
  val bind : ('a, 'p) t -> ('a -> ('b, 'p) t) -> ('b, 'p) t [@@infix ( >>= )]
  val flat_map : ('a -> ('b, 'p) t) -> ('a, 'p) t -> ('b, 'p) t

  val kliesli : ('a -> ('b, 'p) t) -> ('b -> ('c, 'p) t) -> 'a -> ('c, 'p) t
    [@@infix ( >=> )]

  val join : (('a, 'p) t, 'p) t -> ('a, 'p) t
end
[@@deriving typeclass, infix]

module type S1 = sig
  type +'a t

  include Generic with type ('a, _) t := 'a t
  (** @inline *)
end

module type S2 = sig
  type (+'a, 'p) t

  include Generic with type ('a, 'p) t := ('a, 'p) t
  (** @inline *)
end

module type Syntax = sig
  type +'a t

  include Functor.SYNTAX with type 'a t := 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntax alias of {!S.bind}. *)
end

module type Reader = sig
  (** Computations that read values from a shared environment. *)

  include S1

  type e
  (** The type of inputs to reader actions. *)

  val run : 'a t -> e -> 'a
  (** Runs a {!'a t} and extracts the final value ['a] from it. *)

  val ask : e t
  (** Retrieves the monad environment. *)

  val asks : (e -> 'a) -> 'a t
  (** Retrieves a projection of the current monad environment. *)

  val local : (e -> e) -> 'a t -> 'a t
  (** [local f m] executes a computation in [m] in an environment modified by
      [f]. *)
end

module type Reader_open = sig
  type (+'a, +'e) t

  include Generic with type ('a, 'e) t := ('a, 'e) t

  val run : ('a, 'e) t -> 'e -> 'a
  val ask : ('e, 'e) t
  val asks : ('e -> 'a) -> ('a, 'e) t
  val local : ('e -> 'e) -> ('a, 'e) t -> ('a, 'e) t
end

module type Writer = sig
  include S1

  type w
  (** The type of outputs emitted by writer actions. *)

  val tell : w -> unit t
  (** [tell w] is an action producing the output [w]. *)

  val listen : 'a t -> 'a t
  (** [listen m] is an action that executes the action [m] and the output of the
      action as well as its output. *)

  val run : 'a t -> 'a * w
  (** Run a writer action and return a (result, output) pair. *)

  val exec : 'a t -> w
  (** Run a write action and return its output. *)

  val writer : 'a -> w -> 'a t
  (** Create a simple writer action. (Inverse of {!run}.) *)
end

module type Writer_open = sig
  type (+'a, -'w) t

  include Generic with type ('a, 'w) t := ('a, 'w) t

  val tell : 'w -> (unit, 'w) t
  val listen : ('a, 'w) t -> ('a, 'w) t
  val run : ('a, 'w) t -> 'a * 'w
  val exec : ('a, 'w) t -> 'w
  val writer : 'a -> 'w -> ('a, 'w) t
end

module type State = sig
  include S1

  type s
  (** The type of state underlying state actions. *)
end

module type INFIX1 = INFIX

module type Monad = sig
  (** Monads must satisfy the following three laws:

      - {b left identity}. [(return a >>= f) ≡ (f a)];
      - {b right identity}. [(m >>= return) ≡ m];
      - {b associativity}. [((m >>= f) >>= g) ≡ (m >>= (f >=> g))]. *)

  type nonrec 'a t = 'a t

  module type Minimal = Minimal
  module type Generic = Generic
  module type S1 = S1
  module type S2 = S2
  module type INFIX1 = INFIX
  module type Syntax = Syntax

  module Of_minimal (X : Minimal) : Generic with type ('a, 'p) t = ('a, 'p) X.t

  (** {1 Standard monad instances} *)

  (* module Identity : S with type 'a t = 'a *)

  (* module type READER = READER
   * 
   * module Make_reader (E : T.T) : READER with type e := E.t
   * 
   * module type WRITER = WRITER
   * 
   * module Make_writer (W : Monoid.S) : WRITER with type w := W.t
   * 
   * module type STATE = STATE
   * 
   * module Make_state (S : T.T) : STATE with type s := S.t *)
end
