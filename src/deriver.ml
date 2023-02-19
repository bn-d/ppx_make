open Ppxlib

(* generate function expression for option *)
let fun_expression_of_option ~loc (ct : core_type) : expression =
  Ast_helper.with_default_loc loc (fun () ->
      match Utils.get_attributes ct.ptyp_attributes with
      | Default e -> [%expr fun ?(value = [%e e]) () -> Some value]
      | No_attr -> [%expr fun ?value () -> value]
      | _ ->
          Location.raise_errorf ~loc
            "option types only support `default` attribute")

(* generate function expression for record *)
let fun_expression_of_record ~loc ?choice (lds : label_declaration list) :
    expression =
  Ast_helper.with_default_loc loc (fun () ->
      let attrs : Utils.attr_type list =
        List.map (fun ld -> Utils.get_attributes ld.pld_attributes) lds
      in
      let label_pats, main_pats =
        List.fold_left2
          (fun (label_pats, main_pats) { pld_name; pld_type; pld_loc; _ }
               (attr : Utils.attr_type) ->
            let loc = pld_loc in
            Ast_helper.with_default_loc loc (fun () ->
                let pat = Ast_helper.Pat.var pld_name
                and optional = Utils.is_core_type_optional pld_type in
                match (attr, optional) with
                | Main, _ -> (label_pats, pat :: main_pats)
                | Default def, _ ->
                    let arg_label = Optional pld_name.txt in
                    ((arg_label, Some def, pat) :: label_pats, main_pats)
                | No_attr, true ->
                    let arg_label = Optional pld_name.txt
                    and def =
                      Utils.default_expression_of_core_type ~loc pld_type
                    in
                    ((arg_label, def, pat) :: label_pats, main_pats)
                | _, _ ->
                    let arg_label = Labelled pld_name.txt in
                    ((arg_label, None, pat) :: label_pats, main_pats)))
          ([], []) lds attrs
      in
      let main_pats =
        if main_pats = [] then [ Ast_helper.Pat.any () ] else main_pats
      in
      lds
      |> List.map2
           (fun attr { pld_name; pld_type; pld_loc; _ } ->
             let loc = pld_loc in
             Ast_helper.with_default_loc loc (fun () ->
                 let lid = Utils.longident_loc_of_name pld_name in
                 let option_ = Utils.is_core_type_option pld_type
                 and expr = Ast_helper.Exp.ident lid in
                 match (attr, option_) with
                 | Utils.Default _, true -> (lid, [%expr Some [%e expr]])
                 | _, _ -> (lid, expr)))
           attrs
      |> (fun labels -> Ast_helper.Exp.record labels None)
      |> Utils.add_choice_to_expr choice
      |> fun expr ->
      List.fold_left
        (fun acc cur_pat -> Ast_helper.Exp.fun_ Nolabel None cur_pat acc)
        expr main_pats
      |> fun expr ->
      List.fold_left
        (fun acc (arg_label, default_expr, cur_pat) ->
          Ast_helper.Exp.fun_ arg_label default_expr cur_pat acc)
        expr label_pats)

(* generate function expression for tuple *)
let fun_expression_of_tuple ~loc ?choice (cts : core_type list) : expression =
  Ast_helper.with_default_loc loc (fun () ->
      let arg_types =
        List.mapi
          (fun idx ({ ptyp_loc = loc; ptyp_attributes = attrs; _ } as ct) ->
            let name = Utils.gen_tuple_label ~loc idx in
            Arg_type.of_core_type ~loc ~attrs name ct)
          cts
      in
      arg_types
      |> List.map (fun (name, _, _) ->
             Ast_helper.with_default_loc name.loc (fun () ->
                 let lid = Utils.longident_loc_of_name name in
                 Ast_helper.Exp.ident lid))
      |> (fun exprs ->
           match exprs with [ expr ] -> expr | _ -> Ast_helper.Exp.tuple exprs)
      |> Utils.add_choice_to_expr choice
      |> Ast_helper.Exp.fun_ Nolabel None (Ast_helper.Pat.any ())
      |> fun expr ->
      List.fold_left
        (fun acc arg_type ->
          let arg_label, default_expr, pat, _ =
            Arg_type.to_asttypes ~is_tuple:true arg_type
          in
          Ast_helper.Exp.fun_ arg_label default_expr pat acc)
        expr (List.rev arg_types))

(* generate function core type for option *)
let fun_core_type_of_option ~loc (name, params) (in_ct : core_type) : core_type
    =
  Ast_helper.with_default_loc loc (fun () ->
      let return_ct = Utils.core_type_of_name ~params name in
      [%type: ?value:[%t in_ct] -> unit -> [%t return_ct]])

(* generate function core type for record *)
let fun_core_type_of_record ~loc (name, params) (lds : label_declaration list) :
    core_type =
  Ast_helper.with_default_loc loc (fun () ->
      let arg_types =
        List.map
          (fun { pld_loc = loc; pld_name; pld_type; pld_attributes = attrs; _ } ->
            Arg_type.of_core_type ~loc ~attrs pld_name pld_type)
          lds
        |> List.rev
      in
      let main_cts =
        List.map
          (function _, ct, Arg_type.Nolabel -> Some ct | _ -> None)
          arg_types
        |> List.filter Option.is_some
        |> List.map Option.get
      in
      let main_cts =
        if main_cts = [] then [ Utils.unit_core_type ~loc ] else main_cts
      in
      name
      |> Utils.core_type_of_name ~params
      |> fun ct ->
      List.fold_left
        (fun acc cur -> Ast_helper.Typ.arrow Nolabel cur acc)
        ct main_cts
      |> fun ct ->
      List.fold_left
        (fun acc arg_type ->
          match arg_type with
          | _, _, Arg_type.Nolabel -> acc
          | _ ->
              let arg_label, _, _, ct = Arg_type.to_asttypes arg_type in
              Ast_helper.Typ.arrow arg_label ct acc)
        ct arg_types)

(* generate function core type for tuple *)
let fun_core_type_of_tuple ~loc (name, params) (cts : core_type list) :
    core_type =
  Ast_helper.with_default_loc loc (fun () ->
      let arg_types =
        List.mapi
          (fun idx ({ ptyp_loc = loc; ptyp_attributes = attrs; _ } as ct) ->
            let name = Utils.gen_tuple_label ~loc idx in
            Arg_type.of_core_type ~loc ~attrs name ct)
          cts
      in
      name
      |> Utils.core_type_of_name ~params
      |> Ast_helper.Typ.arrow Nolabel (Utils.unit_core_type ~loc)
      |> fun ct ->
      List.fold_left
        (fun acc arg_type ->
          let arg_label, _, _, ct =
            Arg_type.to_asttypes ~is_tuple:true arg_type
          in
          Ast_helper.Typ.arrow arg_label ct acc)
        ct (arg_types |> List.rev))

(* generate structure item for core type *)
let str_item_of_core_type (name, params) (ct : core_type) : structure_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      let pat = Ast_helper.Pat.var @@ Utils.gen_make_name name
      and fun_ct, expr =
        match ct.ptyp_desc with
        | Ptyp_tuple cts ->
            (* T1 * ... * Tn *)
            ( fun_core_type_of_tuple ~loc (name, params) cts,
              fun_expression_of_tuple ~loc cts )
        | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ])
        | Ptyp_constr ({ txt = Ldot (Lident "Option", "t"); _ }, [ in_ct ]) ->
            (* T option *)
            ( fun_core_type_of_option ~loc (name, params) in_ct,
              fun_expression_of_option ~loc ct )
        | _ -> Utils.unsupported_error "core type" name
      in
      [%stri let ([%p pat] : [%t fun_ct]) = [%e expr]])

(* generate structure item for record *)
let str_item_of_record ~loc (name, params) (lds : label_declaration list) :
    structure_item =
  Ast_helper.with_default_loc loc (fun () ->
      let pat = Ast_helper.Pat.var @@ Utils.gen_make_name name
      and ct = fun_core_type_of_record ~loc (name, params) lds
      and expr = fun_expression_of_record ~loc lds in
      [%stri let ([%p pat] : [%t ct]) = [%e expr]])

(* generate structure item for a choice in variant *)
let str_item_of_variant_choice (name, params) (cd : constructor_declaration) :
    structure_item =
  let loc = cd.pcd_loc in
  Ast_helper.with_default_loc loc (fun () ->
      if cd.pcd_res != None then
        Utils.unsupported_error "constructor declaration" name
      else
        let pat =
          Ast_helper.Pat.var @@ Utils.gen_make_choice_name name cd.pcd_name
        and ct, expr =
          match cd.pcd_args with
          | Pcstr_tuple [] ->
              let exp =
                let lid = Utils.longident_loc_of_name cd.pcd_name in
                Ast_helper.Exp.construct lid None
              in
              ( fun_core_type_of_tuple ~loc (name, params) [],
                [%expr fun () -> [%e exp]] )
          | Pcstr_tuple cts ->
              ( fun_core_type_of_tuple ~loc (name, params) cts,
                fun_expression_of_tuple ~loc ~choice:cd.pcd_name cts )
          | Pcstr_record lds ->
              ( fun_core_type_of_record ~loc (name, params) lds,
                fun_expression_of_record ~loc ~choice:cd.pcd_name lds )
        in
        [%stri let ([%p pat] : [%t ct]) = [%e expr]])

(* generate signature item for core type *)
let sig_item_of_core_type (name, params) (ct : core_type) : signature_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      (match ct.ptyp_desc with
      | Ptyp_tuple cts ->
          (* T1 * ... * Tn *)
          fun_core_type_of_tuple ~loc (name, params) cts
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ])
      | Ptyp_constr ({ txt = Ldot (Lident "Option", "t"); _ }, [ in_ct ]) ->
          (* T option *)
          fun_core_type_of_option ~loc (name, params) in_ct
      | _ -> Utils.unsupported_error "core type" name)
      |> Ast_helper.Val.mk @@ Utils.gen_make_name name
      |> Ast_helper.Sig.value)

(* generate signature item for record *)
let sig_item_of_record ~loc (name, params) (lds : label_declaration list) :
    signature_item =
  Ast_helper.with_default_loc loc (fun () ->
      lds
      |> fun_core_type_of_record ~loc (name, params)
      |> Ast_helper.Val.mk @@ Utils.gen_make_name name
      |> Ast_helper.Sig.value)

(* generate signature item for a choice of variant *)
let sig_item_of_variant_choice (name, params) (cd : constructor_declaration) :
    signature_item =
  let loc = cd.pcd_loc in
  Ast_helper.with_default_loc loc (fun () ->
      if cd.pcd_res != None then
        Utils.unsupported_error "constructor declaration" name
      else
        (match cd.pcd_args with
        | Pcstr_tuple cts -> fun_core_type_of_tuple ~loc (name, params) cts
        | Pcstr_record lds -> fun_core_type_of_record ~loc (name, params) lds)
        |> Ast_helper.Val.mk @@ Utils.gen_make_choice_name name cd.pcd_name
        |> Ast_helper.Sig.value)

(* generate structure for type declaration *)
let structure_of_type_decl ~loc:_ (_rec_flag : rec_flag) (td : type_declaration)
    : structure =
  let loc = td.ptype_loc
  and name = td.ptype_name
  and params = Utils.params_core_type_of_type_decl td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ str_item_of_core_type (name, params) ct ]
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      List.map (str_item_of_variant_choice (name, params)) cds
  | { ptype_kind = Ptype_record lds; _ } ->
      (* type t = {l: T; ...} *)
      [ str_item_of_record ~loc (name, params) lds ]
  | _ -> Utils.unsupported_error "type declaration" name

(* generate signature for type declaration *)
let signature_of_type_decl ~loc:_ (_rec_flag : rec_flag) (td : type_declaration)
    : signature =
  let loc = td.ptype_loc
  and name = td.ptype_name
  and params = Utils.params_core_type_of_type_decl td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ sig_item_of_core_type (name, params) ct ]
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      List.map (sig_item_of_variant_choice (name, params)) cds
  | { ptype_kind = Ptype_record lds; _ } ->
      (* type t = {l: T; ...} *)
      [ sig_item_of_record ~loc (name, params) lds ]
  | _ -> Utils.unsupported_error "type declaration" name

let str_type_decl = Utils.make_type_decl_generator structure_of_type_decl
let sig_type_decl = Utils.make_type_decl_generator signature_of_type_decl
let deriver = Deriving.add "make" ~str_type_decl ~sig_type_decl
