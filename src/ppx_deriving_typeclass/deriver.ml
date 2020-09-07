open Ppxlib

module type S = sig
  val derive_typeclass : module_type_declaration -> structure
end

let higher x = Ldot (lident "Brands", x)

module Located (A : Ast_builder.S) : S = struct
  open A

  let get_type_arity = function
    | {
        psig_desc =
          Psig_type
            (_rec, { ptype_name = { txt = "t"; _ }; ptype_params; _ } :: _);
        _;
      } ->
        List.length ptype_params
    | _ -> failwith "Could not get type arity"

  let type_apply f a = ptyp_constr (Located.mk (higher "app")) [ a; f ]

  let type_variables_of =
    object
      inherit [label loc list] Ast_traverse.fold_map as super

      method! core_type t acc =
        let t, acc = super#core_type t acc in
        let t =
          match t.ptyp_desc with
          | Ptyp_constr ({ txt = Lident "t"; _ }, vars) ->
              vars |> List.fold_left (fun t v -> type_apply t v) (ptyp_var "t")
          | _ -> t
        in
        let acc =
          match t.ptyp_desc with
          | Ptyp_var l when l <> "t" -> Located.mk l :: acc
          | _ -> acc
        in
        (t, acc)
    end

  let record_field_of_value_description { pval_name; pval_type; _ } =
    let type_ =
      (* Track type-variables that need to be explicitly [forall] quantified in
         the record field *)
      let typ, tvars = type_variables_of#core_type pval_type [] in
      ptyp_poly tvars typ
    in

    label_declaration ~name:pval_name ~mutable_:Asttypes.Immutable ~type_

  (** Traverse the module type, replacing all instances of ['a t] with
      [('a, br) app]. *)
  let derive_typeclass { pmtd_type; _ } =
    match pmtd_type with
    | Some { pmty_desc = Pmty_signature desc; _ } -> (
        match desc with
        | typ :: fields ->
            let _arity = get_type_arity typ in
            let label_declarations =
              fields
              |> List.filter_map (fun { psig_desc; _ } ->
                     match psig_desc with
                     | Psig_value v ->
                         Some (record_field_of_value_description v)
                     (* Ignorable *)
                     | Psig_exception _ | Psig_open _ | Psig_attribute _
                     | Psig_extension _ ->
                         None
                     (* Extra types *)
                     | Psig_type _ | Psig_typesubst _ | Psig_typext _ ->
                         failwith "Cannot support more than one type"
                     (* Nested *)
                     | Psig_module _ | Psig_modsubst _ | Psig_recmodule _
                     | Psig_include _ | Psig_modtype _ ->
                         failwith "Cannot support nested modules"
                     (* Classes *)
                     | Psig_class _ | Psig_class_type _ ->
                         failwith "Class types unsupported")
            in
            let typeclass_decl =
              type_declaration ~name:(Located.mk "t")
                ~params:[ (ptyp_var "t", Invariant) ]
                ~cstrs:[] ~private_:Public ~manifest:None
                ~kind:(Ptype_record label_declarations)
            in
            [ pstr_type Recursive [ typeclass_decl ] ]
        | [] -> failwith "Empty module type signature" )
    | None -> failwith "Abstract module type"
    | Some _ -> failwith "Unsupported module type definition"
end
