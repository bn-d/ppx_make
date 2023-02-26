open Ppxlib

type attr = No_attr | Main | Required | Default of expression
type label_arg = Labelled | Optional of expression option
type t = string loc * core_type * label_arg option

let default_expression_of_core_type ~loc (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, _)
  | Ptyp_constr ({ txt = Ldot (Lident "Option", "t"); _ }, _) ->
      Some None
  | Ptyp_constr ({ txt = Lident "list"; _ }, _)
  | Ptyp_constr ({ txt = Ldot (Lident "List", "t"); _ }, _) ->
      Some (Some [%expr []])
  | Ptyp_constr ({ txt = Lident "array"; _ }, _)
  | Ptyp_constr ({ txt = Ldot (Lident "Array", "t"); _ }, _) ->
      Some (Some [%expr Array.of_list []])
  | Ptyp_constr ({ txt = Lident "string"; _ }, [])
  | Ptyp_constr ({ txt = Ldot (Lident "String", "t"); _ }, _) ->
      Some (Some [%expr ""])
  | _ -> None

let get_attr (attrs : attribute list) =
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
      | "main" | "make.main" -> Main
      | "required" | "make.required" -> Required
      | "default" | "make.default" -> (
          match attr.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Default expr
          | _ ->
              Location.raise_errorf ~loc:attr.attr_loc
                "value in default attribute is not supported")
      (* ignore unknown attrs *)
      | _ -> No_attr)
      |> check_res ~loc:attr.attr_loc acc)
    No_attr attrs

let of_core_type ~loc ~attrs name ct =
  let attr_type = get_attr attrs in
  match attr_type with
  | No_attr -> (
      match default_expression_of_core_type ~loc ct with
      | Some None ->
          let ct = Utils.strip_option ct in
          (name, ct, Some (Optional None))
      | Some def -> (name, ct, Some (Optional def))
      | None -> (name, ct, Some Labelled))
  | Required -> (name, ct, Some Labelled)
  | Default def -> (name, ct, Some (Optional (Some def)))
  | Main -> (name, ct, None)

let label_to_asttypes (name, ct, arg_types) =
  let pat = Ast_helper.Pat.var ~loc:name.loc name in
  match arg_types with
  | Some Labelled -> (Asttypes.Labelled name.txt, None, pat, ct)
  | Some (Optional def_expr) -> (Optional name.txt, def_expr, pat, ct)
  | _ -> Location.raise_errorf ~loc:name.loc "unexpected error"

let split ts =
  let labels, mains = List.partition (fun (_, _, l) -> l <> None) ts in
  let labels =
    List.map (fun (name, ct, l) -> label_to_asttypes (name, ct, l)) labels
  and mains =
    List.map
      (fun (name, ct, _) ->
        let pat = Ast_helper.Pat.var ~loc:name.loc name in
        (pat, ct))
      mains
  in
  (labels, mains)
