open Ppxlib

module type S = sig
  val derive_infix : module_type_declaration -> structure
end

module Attributes = struct
  let infix =
    Attribute.declare "infix" Attribute.Context.Value_description
      Ast_pattern.(pstr __')
      (function
        | {
            txt =
              [
                {
                  pstr_desc =
                    Pstr_eval
                      ({ pexp_desc = Pexp_ident { txt = Lident l; _ }; _ }, _);
                  _;
                };
              ];
            _;
          } ->
            l
        | { txt = [] | [ _ ] | _ :: _ :: _; loc } ->
            Location.raise_errorf ~loc
              "`infix' payload must be a single infix operator name")
end

module Located (A : Ast_builder.S) : S = struct
  open A

  let docstring ~source_module_type_name ~prefix_name : attribute =
    let doc =
      Format.sprintf "Infix alias of {!%s.%s}." source_module_type_name
        prefix_name
    in
    attribute ~name:(Located.mk "ocaml.doc")
      ~payload:(PStr [ pstr_eval (estring doc) [] ])

  (** Traverse the module type, replacing all instances of ['a t] with
      [('a, br) app]. *)
  let derive_infix { pmtd_type; pmtd_name; _ } =
    match pmtd_type with
    | Some { pmty_desc = Pmty_signature items; _ } ->
        items
        |> List.filter_map (fun item ->
               match item.psig_desc with
               | Psig_value value_desc -> (
                   (* Create the infix equivalent, and attach a docstring
                      explaining the alias *)
                   match Attribute.get Attributes.infix value_desc with
                   | Some name ->
                       Some
                         (psig_value
                            {
                              value_desc with
                              pval_name = Located.mk name;
                              pval_attributes =
                                [
                                  docstring
                                    ~source_module_type_name:pmtd_name.txt
                                    ~prefix_name:value_desc.pval_name.txt;
                                ];
                            })
                   | None -> None )
               | Psig_type _ -> Some item
               (* Ignored signature items *)
               | Psig_typesubst _ | Psig_typext _ -> assert false
               | Psig_exception _ | Psig_attribute _ | Psig_open _
               | Psig_extension _ ->
                   None
               (* Nested *)
               | Psig_module _ | Psig_modsubst _ | Psig_include _
               | Psig_recmodule _ | Psig_modtype _ ->
                   failwith "Nested modules unsupported"
               (* Classes *)
               | Psig_class _ | Psig_class_type _ ->
                   failwith "Class types unsupported")
        |> pmty_signature
        |> (fun type_ ->
             module_type_declaration ~name:(Located.mk "INFIX")
               ~type_:(Some type_))
        |> pstr_modtype
        |> fun mtype -> [ mtype ]
    | None -> failwith "Abstract module type"
    | Some _ -> failwith "Unsupported module type definition"
end
