open Ppxlib

module type S = sig
  val map_test_module : payload -> structure_item
end

module Located (A : Ast_builder.S) : S = struct
  open A

  let collect_test_names s =
    (object
       inherit [string list] Ast_traverse.fold_map as super

       method! structure_item t acc =
         let t, acc = super#structure_item t acc in
         match t.pstr_desc with
         | Pstr_extension
             ( ( { txt = "test" | "t"; _ },
                 PStr [ ({ pstr_desc = Pstr_value (_, [ t ]); _ } as payload) ]
               ),
               _ ) -> (
             match t.pvb_pat.ppat_desc with
             | Ppat_var { txt; _ } -> (payload, txt :: acc)
             | _ -> assert false )
         | _ -> (t, acc)
    end)
      #structure
      s []
    |> fun (s, acc) -> (s, List.rev acc)

  let rec meta_list : expression list -> expression = function
    | [] -> [%expr []]
    | x :: xs -> [%expr [%e x] :: [%e meta_list xs]]

  let test_of_name name =
    [%expr Alcotest.test_case [%e estring name] `Quick [%e evar name]]

  let get_suite ~module_name s =
    let s, names = collect_test_names s in
    let suite = names |> List.map test_of_name |> meta_list in
    ( s @ [ [%stri let __ppx_alcotest_suite = [%e suite]] ],
      [%stri
        let __ppx_alcotest_suite =
          ( [%e A.estring module_name],
            [%e evar (module_name ^ ".__ppx_alcotest_suite")] )
          :: __ppx_alcotest_suite] )

  let map_test_module = function
    | PStr
        [
          {
            pstr_desc =
              Pstr_module
                {
                  pmb_name = { txt = Some module_name; _ };
                  pmb_expr = { pmod_desc = Pmod_structure module_items; _ };
                  _;
                };
            _;
          };
        ] ->
        let s, extr = get_suite ~module_name module_items in
        pstr_include
        @@ include_infos
        @@ pmod_structure
        @@ [
             pstr_module
             @@ module_binding
                  ~name:(Located.mk (Some module_name))
                  ~expr:(pmod_structure s);
             extr;
           ]
    | PStr [ ({ pstr_desc = Pstr_value (_, _); _ } as a) ] -> a
    | _ -> failwith "invalid"
end

let () =
  let rule name =
    Extension.declare name Extension.Context.Structure_item
      Ast_pattern.(__)
      (fun ~loc ~path:_ ->
        let (module A) = Ast_builder.make loc in
        let (module L) = (module Located (A) : S) in
        L.map_test_module)
    |> Context_free.Rule.extension
  in
  Driver.register_transformation
    ~rules:[ rule "alcotest.test"; rule "alcotest.t" ]
    ~enclose_impl:(fun loc ->
      let loc = match loc with Some l -> l | None -> Location.none in
      ( [ [%stri let __ppx_alcotest_suite = []] ],
        [
          [%stri
            let () =
              Alcotest.run __FILE__ (Stdlib.List.rev __ppx_alcotest_suite)];
        ] ))
    "alcotest.test"

(*   let () =
 *     let rule =
 *       Extension.declare "alcotest.check" Extension.Context.expression
 *     Ast_pattern.(__)
 * Driver.register_transformation
 *    ~rule:[ rule ]
 *   "alcotest.check" *)
