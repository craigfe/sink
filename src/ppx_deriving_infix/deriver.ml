open Ppxlib

let split_at_index : int -> string -> string * string =
 fun i s -> (String.sub s 0 i, String.sub s i (String.length s - i))

let split_integer_suffix s : string * int option =
  let last = String.length s - 1 in
  let digit_at_index i =
    let c = s.[i] in
    48 <= Char.code c && Char.code c < 58
  in
  let rec aux = function
    | -1 -> ("", s) (* The whole string is an integer *)
    | n when digit_at_index n -> aux (n - 1)
    | n -> split_at_index (n + 1) s
  in
  if digit_at_index last then
    let prefix, suffix = aux last in
    (prefix, Some (int_of_string suffix))
  else (s, None)

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

  type item_result = {
    infix_lets : structure_item list;
    infix_vals : signature_item list;
    tdecls : type_declaration list;
  }
  (** For each item annotated with \[\@\@infix ... \], we build two infix
      aliases, one using [let] and one using [val]. *)

  let add_to_acc ?infix_let ?infix_val ?tdecl acc =
    let list_of_option = function Some x -> [ x ] | None -> [] in
    {
      infix_lets = list_of_option infix_let @ acc.infix_lets;
      infix_vals = list_of_option infix_val @ acc.infix_vals;
      tdecls = list_of_option tdecl @ acc.tdecls;
    }

  (** {[ let ( >>= ) = S.bind ]} *)
  let str_value ~prefix_name ~infix_name =
    pstr_value Nonrecursive
      [
        value_binding
          ~pat:(ppat_var (Located.mk infix_name))
          ~expr:(pexp_ident (Located.lident ("X." ^ prefix_name)));
      ]

  (** {[
        module Make_infix (X : S) : INFIX with type 'a t := 'a X.t = struct ...end
      ]} *)
  let wrap_str_aliases ~index_suffix ~tdecls ~prefix_mtyp_name str_items =
    let constraint_of_td td =
      let td = name_type_params_in_td td in
      let for_subst =
        type_declaration ~name:td.ptype_name ~params:td.ptype_params ~cstrs:[]
          ~private_:Public ~kind:Ptype_abstract
          ~manifest:
            (Some
               (ptyp_constr
                  (Located.map_lident td.ptype_name)
                  (List.map fst td.ptype_params)))
      in
      Pwith_typesubst (Located.map_lident for_subst.ptype_name, for_subst)
    in
    let t_aliases =
      tdecls
      |> List.map (fun tdecl ->
             let constr =
               ptyp_constr
                 (Located.lident ("X." ^ tdecl.ptype_name.txt))
                 (List.map fst tdecl.ptype_params)
             in

             pstr_type Nonrecursive
               [
                 {
                   tdecl with
                   ptype_kind = Ptype_abstract;
                   ptype_manifest = Some constr;
                 };
               ])
    in
    let _constraints = tdecls |> List.map constraint_of_td in

    pstr_module
      (module_binding
         ~name:(Located.mk ("Make_infix" ^ index_suffix))
         ~expr:
           (pmod_functor (Located.mk "X")
              (Some (pmty_ident (Located.lident prefix_mtyp_name)))
              ((* pmod_constraint *)
               pmod_structure
                 (t_aliases @ str_items)
                 (* (pmty_with (pmty_ident (Located.lident "INFIX")) constraints) *))))

  (** {[
        val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
        (** Infix alias of {!S.bind}. *)
      ]} *)
  let sig_value ~type_ ~prefix_name ~infix_name ~prefix_mtyp_name =
    let doc =
      Format.sprintf "Infix alias of {!%s.%s}." prefix_mtyp_name prefix_name
    in
    let v_desc =
      value_description ~name:(Located.mk infix_name) ~type_ ~prim:[]
    in
    psig_value
      {
        v_desc with
        pval_attributes =
          [
            attribute ~name:(Located.mk "ocaml.doc")
              ~payload:(PStr [ pstr_eval (estring doc) [] ]);
          ];
      }

  (** {[ module type INFIX = sig ... end ]} *)
  let wrap_sig_aliases ~index_suffix sig_items =
    let type_ = Some (pmty_signature sig_items) in
    module_type_declaration ~name:(Located.mk ("INFIX" ^ index_suffix)) ~type_
    |> pstr_modtype

  let process_item ~prefix_mtyp_name acc item =
    match item.psig_desc with
    | Psig_value value_desc -> (
        let type_ = value_desc.pval_type
        and prefix_name = value_desc.pval_name.txt in

        match Attribute.get Attributes.infix value_desc with
        | None -> acc
        | Some infix_name ->
            add_to_acc
              ~infix_let:(str_value ~prefix_name ~infix_name)
              ~infix_val:
                (sig_value ~type_ ~prefix_name ~infix_name ~prefix_mtyp_name)
              acc )
    | Psig_type (rec_flag, tdecls) ->
        let tdecl = List.hd tdecls in
        add_to_acc ~infix_val:(psig_type rec_flag tdecls) ~tdecl acc
    (* Ignored signature items *)
    | Psig_typesubst _ | Psig_typext _ -> assert false
    | Psig_exception _ | Psig_attribute _ | Psig_open _ | Psig_extension _ ->
        acc
    (* Nested *)
    | Psig_module _ | Psig_modsubst _ | Psig_include _ | Psig_recmodule _
    | Psig_modtype _ ->
        failwith "Nested modules unsupported"
    (* Classes *)
    | Psig_class _ | Psig_class_type _ -> failwith "Class types unsupported"

  (** Traverse the module type, replacing all instances of ['a t] with
      [('a, br) app]. *)
  let derive_infix { pmtd_type; pmtd_name = { txt = prefix_mtyp_name; _ }; _ } =
    let index_suffix =
      split_integer_suffix prefix_mtyp_name |> snd |> function
      | Some i -> string_of_int i
      | None -> ""
    in
    match pmtd_type with
    | Some { pmty_desc = Pmty_signature items; _ } ->
        let { infix_lets; infix_vals; tdecls } =
          items
          |> List.fold_left
               (process_item ~prefix_mtyp_name)
               { infix_lets = []; infix_vals = []; tdecls = [] }
        in
        let infix_lets = List.rev infix_lets
        and infix_vals = List.rev infix_vals
        and tdecls = List.rev tdecls in
        [
          wrap_sig_aliases ~index_suffix infix_vals;
          wrap_str_aliases ~index_suffix ~tdecls ~prefix_mtyp_name infix_lets;
        ]
    | None -> failwith "Abstract module type"
    | Some _ -> failwith "Unsupported module type definition"
end
