open Ppxlib

(** Given a module type of the form:

    {[
      module type S1 = sig
        type 'a t

        val map : ('a -> 'b) -> 'a t -> 'b t
      end
      [@@deriving phantom]
    ]}

    generate a new module type in which [t] has an extra phantom type parameter
    that is unified across all occurrences:

    {[
      (** Equal to {!S1} but with an additional phantom type parameter. *)
      module type S2 = sig
        type ('a, 'phan) t

        val map : ('a -> 'b) -> ('a, 'phan) t -> ('b, 'phan) t
      end
      [@@deriving phantom]
    ]} *)

let add_phantom_parameter_to (module A : Ast_builder.S) id =
  let tvar = A.ptyp_var "phantom" in
  object
    inherit Ast_traverse.map as super

    method! type_declaration t =
      let t = super#type_declaration t in
      if String.equal t.ptype_name.txt id then
        { t with ptype_params = t.ptype_params @ [ (tvar, Invariant) ] }
      else t

    method! core_type t =
      let t = super#core_type t in
      match t.ptyp_desc with
      | Ptyp_constr (l, vars) when l.txt = Lident id ->
          { t with ptyp_desc = Ptyp_constr (l, vars @ [ tvar ]) }
      | _ -> t
  end

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

(** Parse as large a suffix as possible of a given string as an integer, then
    apply the given function to the result (if at least one digit was parsed). *)
let map_integer_suffix : (int option -> int) -> string -> string =
 fun f s ->
  let prefix, suffix = split_integer_suffix s in
  prefix ^ string_of_int (f suffix)

let add_phantom_parameter ~loc ~path:_
    { pmtd_name; pmtd_type; pmtd_attributes = _; _ } subderiving =
  let (module A) = Ast_builder.make loc in
  let open A in
  let pmtd_name =
    pmtd_name.txt
    |> map_integer_suffix (function Some i -> i + 1 | None -> 1)
    |> Located.mk
  in
  let pmtd_type =
    pmtd_type
    |> Option.map (add_phantom_parameter_to (module A) "t")#module_type
  in
  let pmtd_attributes =
    match subderiving with
    | None -> []
    | Some e ->
        [
          attribute ~name:(Located.mk "deriving")
            ~payload:(PStr [ pstr_eval e [] ]);
        ]
  in
  [ pstr_modtype { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc = A.loc } ]

let branded : Deriving.t =
  let open Deriving in
  let args = Args.(empty +> arg "subderiving" __) in
  add
    ~str_module_type_decl:(Generator.make args add_phantom_parameter)
    "phantom"
