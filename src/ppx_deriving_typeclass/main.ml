open Ppxlib

let expand_module_type ~loc ~path:_ input_ast =
  let (module S) = Ast_builder.make loc in
  let (module D) = (module Deriver.Located (S) : Deriver.S) in
  D.derive_typeclass input_ast

let typeclass =
  let open Deriving in
  add
    ~str_module_type_decl:(Generator.make Args.empty expand_module_type)
    "typeclass"
