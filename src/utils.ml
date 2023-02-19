open Ppxlib

(* Core Type Utils *)

let unit_core_type ~loc =
  Ast_helper.Typ.constr ~loc { txt = Lident "unit"; loc } []

let core_type_of_name ?(params = []) { txt = name; loc } =
  Ast_helper.Typ.constr ~loc { txt = Lident name; loc } params

let is_core_type_option (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, _)
  | Ptyp_constr ({ txt = Ldot (Lident "Option", "t"); _ }, _) ->
      true
  | _ -> false

let is_core_type_list (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "list"; _ }, _)
  | Ptyp_constr ({ txt = Ldot (Lident "List", "t"); _ }, _) ->
      true
  | _ -> false

let is_core_type_array (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "array"; _ }, _)
  | Ptyp_constr ({ txt = Ldot (Lident "Array", "t"); _ }, _) ->
      true
  | _ -> false

let is_core_type_string (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "String", "t"); _ }, _) ->
      true
  | _ -> false

let is_core_type_optional (ct : core_type) =
  is_core_type_option ct
  || is_core_type_list ct
  || is_core_type_array ct
  || is_core_type_string ct

let strip_option (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ])
  | Ptyp_constr ({ txt = Ldot (Lident "Option", "t"); _ }, [ in_ct ]) ->
      in_ct
  | _ -> ct

let default_expression_of_core_type ~loc (ct : core_type) =
  if is_core_type_list ct then
    Some [%expr []]
  else if is_core_type_array ct then
    Some [%expr Array.of_list []]
  else if is_core_type_string ct then
    Some [%expr ""]
  else
    None

type attr_type = No_attr | Main | Required | Default of expression

(* Attributes Utils *)
let get_attributes (attrs : attribute list) =
  let check_res ~loc acc cur =
    match (acc, cur) with
    | No_attr, _ -> cur
    | _, No_attr -> acc
    | _, _ ->
        Location.raise_errorf ~loc
          "single field cannot have more than one attributes"
  in
  List.fold_left
    (fun acc (attr : attribute) ->
      (match attr.attr_name.txt with
      | "main" -> Main
      | "required" -> Required
      | "default" -> (
          match attr.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Default expr
          | _ ->
              Location.raise_errorf ~loc:attr.attr_loc
                "value in default attribute is not supported")
      (* ignore unknown attrs *)
      | _ -> No_attr)
      |> check_res ~loc:attr.attr_loc acc)
    No_attr attrs

(* Misc. Utils *)

let unsupported_error str { txt; loc } =
  Location.raise_errorf ~loc "%s %s cannot be derived" str txt

let make_type_decl_generator f =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (rec_flag, tds) ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      tds |> List.map (f ~loc rec_flag) |> List.concat)

let gen_make_name { txt = name; loc } = { txt = "make_" ^ name; loc }

let gen_make_choice_name { txt = name; _ } { txt = choice_name; loc } =
  let txt = String.lowercase_ascii ("make_" ^ choice_name ^ "_of_" ^ name) in
  { txt; loc }

let gen_tuple_label ~loc index = { txt = "v" ^ string_of_int index; loc }
let longident_loc_of_name { txt; loc } = { txt = Lident txt; loc }

let add_choice_to_expr choice expr =
  match choice with
  | Some choice_name ->
      let lid = longident_loc_of_name choice_name in
      Ast_helper.Exp.construct lid (Some expr)
  | None -> expr

let params_core_type_of_type_decl (td : type_declaration) =
  List.map (fun (ct, _) -> ct) td.ptype_params
