open Ppxlib

(** Given a module type declaration of the form:

    {[
      module type S = sig
        type t

        val foo : t -> int
        val bar : t -> int -> t [@@infix ( <+> )]
      end
      [@@deriving infix]
    ]}

    generate new items of the form:

    {[
      module type INFIX = sig
        type t

        val ( <+> ) : t -> int -> t
      end

      module Make_infix (X : S) : INFIX with type t := X.t = struct ... end
    ]}
    - a new module type with the same types and value descriptions with
      [\[@@infix ... \]] attributes carried over and renamed to the infix form.

    - a coercion from the prefix to the infix form. *)

let expand_module_type ~loc ~path:_ input_ast =
  let (module S) = Ast_builder.make loc in
  let (module D) = (module Deriver.Located (S) : Deriver.S) in
  D.derive_infix input_ast

let typeclass =
  let open Deriving in
  add
    ~str_module_type_decl:(Generator.make Args.empty expand_module_type)
    "infix"
