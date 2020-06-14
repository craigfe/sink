open Ppxlib

(** Given a type declaration of the form:

    {[ type ('a_1, ..., 'a_n) t = ... [@@implements Foo.S, Bar.S] ]}

    generate module type inclusions of the form:

    {[
      include Foo.S<n> with type ('a_1, ..., 'a_n) t := ('a_1, ..., 'a_n) t
      (** @closed *)

      include Bar.S<n> with type ('a_1, ..., 'a_n) t := ('a_1, ..., 'a_n) t
      (** @closed *)
    ]}

    Notes:

    - In the case [n=0], module types of the form [Foo.S] are assumed (no number
      suffix).

    - The [@closed] Odoc attribute is added to avoid flooding the documentation. *)

(** The [@@implements ...] attribute consumes a tuple of module type names
    (which are actually [Pexp_construct]s, but the user doesn't need to know
    that...). *)
let implements =
  Attribute.declare "implements" Attribute.Context.Type_declaration
    Ast_pattern.(pstr __')
    (fun payload ->
      match payload.txt with
      | [
       {
         pstr_desc =
           Pstr_eval ({ pexp_desc = Pexp_construct (interface, None); _ }, _);
         _;
       };
      ] ->
          [ interface.txt ]
      | [
       {
         pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple interfaces; _ }, _);
         _;
       };
      ] ->
          interfaces
          |> List.map (function
               | { pexp_desc = Pexp_construct ({ txt = name; _ }, None); _ } ->
                   name
               | item ->
                   Location.raise_errorf ~loc:payload.loc
                     "Unsupported payload type for `implements`: %a"
                     Pprintast.expression item)
      | _ ->
          Location.raise_errorf ~loc:payload.loc
            "`implements' payload must contain a single tuple. Unsupported \
             payload: %a"
            Pprintast.structure payload.txt)

(** Similar to [Ppxlib.mk_named_sig]. *)
let mk_named_sig (module A : Ast_builder.S) ~sg_name =
  let open A in
  let add_closed_attribute incl =
    let attr =
      attribute ~name:(Located.mk "ocaml.doc")
        ~payload:(PStr [ pstr_eval (estring "@closed") [] ])
    in
    { incl with pincl_attributes = attr :: incl.pincl_attributes }
  in
  function
  | [
      ( { ptype_name = { txt = "t"; _ }; ptype_cstrs = []; ptype_params; _ } as
      td );
    ] ->
      let arity = List.length ptype_params in
      let mty =
        if arity = 0 then sg_name else Printf.sprintf "%s%d" sg_name arity
      in
      let td = name_type_params_in_td td in
      let for_subst =
        type_declaration ~name:td.ptype_name ~params:td.ptype_params ~cstrs:[]
          ~private_:Public ~kind:Ptype_abstract
          ~manifest:
            (Some
               (ptyp_constr
                  (Located.map_lident td.ptype_name)
                  (List.map fst ptype_params)))
      in
      include_infos
        (pmty_with
           (pmty_ident (Located.lident mty))
           [ Pwith_typesubst (Located.lident "t", for_subst) ])
      |> add_closed_attribute
      |> psig_include
  | _ -> assert false

let implements : unit =
  let rule =
    let open Context_free.Rule in
    attr_sig_type_decl implements (fun ~ctxt _rec_flag tdecls implements ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        let (module A) = Ast_builder.make loc in
        tdecls
        |> List.map (fun _tdecl ->
               implements
               |> List.filter_map Fun.id
               |> List.flatten
               |> List.map (fun implement ->
                      mk_named_sig
                        (module A)
                        ~sg_name:(Longident.name implement) tdecls))
        |> List.flatten)
  in
  Driver.register_transformation "implements" ~rules:[ rule ]
