open Ppxlib

(* Core Type Utils *)
let core_type_of_name ?(params = []) { txt = name; loc } =
  Ast_helper.Typ.constr ~loc { txt = Lident name; loc } params

let strip_option (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ])
  | Ptyp_constr ({ txt = Ldot (Lident "Option", "t"); _ }, [ in_ct ]) ->
      in_ct
  | _ -> ct

(* Misc. Utils *)

let unsupported_error str { txt; loc } =
  Location.raise_errorf ~loc "%s %s cannot be derived" str txt

let has_make_attr ({ ptype_attributes; _ } : type_declaration) =
  let is_make = function [%expr make] -> true | _ -> false in
  List.exists
    (fun (attr : attribute) ->
      (attr.attr_name.txt = "deriving" || attr.attr_name.txt = "deriving_inline")
      &&
      match attr.attr_payload with
      | PStr [ { pstr_desc = Pstr_eval ([%expr make], _); _ } ] -> true
      | PStr
          [
            {
              pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple exprs; _ }, _);
              _;
            };
          ] ->
          List.exists is_make exprs
      | _ -> false)
    ptype_attributes

let make_type_decl_generator f =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (rec_flag, tds) ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      tds
      |> List.filter has_make_attr
      |> List.map (f ~loc rec_flag)
      |> List.concat)

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
