module type S1 = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t [@@infix ( >>= )]

  val kliesli : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t [@@infix ( >=> )]
end
[@@deriving typeclass, infix, phantom { subderiving = infix }]

module type S = S1

module type SYNTAX = sig
  type 'a t

  include Functor.SYNTAX with type 'a t := 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntax alias of {!S.bind}. *)
end

module type READER = sig
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

module type WRITER = sig
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

module type STATE = sig
  include S

  type s
  (** The type of state underlying state actions. *)
end

module type Monad = sig
  (** Monads must satisfy the following three laws:

      - {b left identity}. [(return a >>= f) ≡ (f a)];
      - {b right identity}. [(m >>= return) ≡ m];
      - {b associativity}. [((m >>= f) >>= g) ≡ (m >>= (f >=> g))]. *)

  module type S = S

  type nonrec 'a t = 'a t

  module type S1 = S1

  module type S2 = S2

  module type INFIX1 = INFIX1

  module type INFIX2 = INFIX2

  module type SYNTAX = SYNTAX

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
