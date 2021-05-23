module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper
module Utils = Ppx_make_utils

let fun_expression_of_record ~loc (lds : P.label_declaration list) :
    P.expression =
  Ast_helper.with_default_loc loc (fun () ->
      let attrs : Utils.attr_type list =
        lds
        |> List.map (fun ld -> ld.P.pld_attributes)
        |> List.map Utils.get_attributes
      in
      let label_pats, main_pats =
        lds
        |> List.fold_left
             (fun (label_pats, main_pats)
                  P.{ pld_name; pld_type; pld_attributes; _ } ->
               let pat = Ast_helper.Pat.var pld_name in
               let attr_type : Utils.attr_type =
                 Utils.get_attributes pld_attributes
               in
               let optional = Utils.is_core_type_optional pld_type in
               match (attr_type, optional) with
               | Main, _ -> (label_pats, pat :: main_pats)
               | Default def, _ ->
                   ( (P.Optional pld_name.txt, Some def, pat) :: label_pats,
                     main_pats )
               | No_attr, true ->
                   let def =
                     let open P in
                     if Utils.is_core_type_list pld_type then Some [%expr []]
                     else if Utils.is_core_type_string pld_type then
                       Some [%expr ""]
                     else None
                   in
                   ((P.Optional pld_name.txt, def, pat) :: label_pats, main_pats)
               | _, _ ->
                   ((P.Labelled pld_name.txt, None, pat) :: label_pats, main_pats))
             ([], [])
      in
      let main_pats =
        match main_pats with [] -> [ Ast_helper.Pat.any () ] | _ -> main_pats
      in
      lds
      |> List.map2
           (fun attr P.{ pld_name; pld_type; pld_loc; _ } ->
             Ast_helper.with_default_loc pld_loc (fun () ->
                 let option_ = Utils.is_core_type_option pld_type in
                 let lid = Utils.longident_loc_of_name pld_name in
                 let expr = Ast_helper.Exp.ident lid in
                 match (attr, option_) with
                 | Utils.Default _, true ->
                     let open P in
                     (lid, [%expr Some [%e expr]])
                 | _, _ -> (lid, expr)))
           attrs
      |> fun labels ->
      Ast_helper.Exp.record labels None |> fun expr ->
      List.fold_left
        (fun acc cur_pat -> Ast_helper.Exp.fun_ P.Nolabel None cur_pat acc)
        expr main_pats
      |> fun expr ->
      List.fold_left
        (fun acc (arg_label, default_expr, cur_pat) ->
          Ast_helper.Exp.fun_ arg_label default_expr cur_pat acc)
        expr label_pats)

let fun_core_type_of_record ~loc name (lds : P.label_declaration list) :
    P.core_type =
  lds
  |> List.fold_left
       (fun (label_list, main_list) P.{ pld_name; pld_type; pld_attributes; _ } ->
         let attr_type : Utils.attr_type =
           Utils.get_attributes pld_attributes
         in
         let optional = Utils.is_core_type_optional pld_type in
         match (attr_type, optional) with
         | Main, _ -> (label_list, pld_type :: main_list)
         | Default _, _ | No_attr, true ->
             let ct = Utils.strip_option pld_type in
             ((P.Optional pld_name.txt, ct) :: label_list, main_list)
         | _, _ -> ((P.Labelled pld_name.txt, pld_type) :: label_list, main_list))
       ([], [])
  |> fun (label_list, main_list) ->
  let main_list =
    match main_list with [] -> [ Utils.unit_core_type ~loc ] | _ -> main_list
  in
  name |> Utils.core_type_of_name |> fun ct ->
  List.fold_left
    (fun acc cur -> Ast_helper.Typ.arrow P.Nolabel cur acc)
    ct main_list
  |> fun ct ->
  List.fold_left
    (fun acc (arg_label, cur) -> Ast_helper.Typ.arrow arg_label cur acc)
    ct label_list

let str_item_of_core_type name (ct : P.core_type) : P.structure_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          (* T option *)
          let pat = Ast_helper.Pat.var @@ Utils.gen_make_name name in
          let return_ct = Utils.core_type_of_name name in
          (* TODO add default *)
          let open P in
          [%stri
            let ([%p pat] : ?value:[%t in_ct] -> unit -> [%t return_ct]) =
             fun ?value () -> value]
      | _ -> Utils.unsupported_error name)

let str_item_of_record ~loc name (lds : P.label_declaration list) :
    P.structure_item =
  Ast_helper.with_default_loc loc (fun () ->
      let pat = Ast_helper.Pat.var @@ Utils.gen_make_name name in
      let ct = fun_core_type_of_record ~loc name lds in
      let expr = fun_expression_of_record ~loc lds in
      let open P in
      [%stri let ([%p pat] : [%t ct]) = [%e expr]])

let sig_item_of_core_type name (ct : P.core_type) : P.signature_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          (* T option *)
          let fun_name = Utils.gen_make_name name in
          let return_ct = Utils.core_type_of_name name in
          let open P in
          [%type: ?value:[%t in_ct] -> unit -> [%t return_ct]]
          |> Ast_helper.Val.mk fun_name |> Ast_helper.Sig.value
      | _ -> Utils.unsupported_error name)

let sig_item_of_record ~loc name (lds : P.label_declaration list) :
    P.signature_item =
  Ast_helper.with_default_loc loc (fun () ->
      let fun_name = Utils.gen_make_name name in
      lds
      |> fun_core_type_of_record ~loc name
      |> Ast_helper.Val.mk fun_name |> Ast_helper.Sig.value)

let structure_item_of_type_decl ~loc:_ (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.structure_item =
  let name = td.ptype_name in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      str_item_of_core_type name ct
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record lds; _ } ->
      (* type t = {l: T; ...} *)
      str_item_of_record ~loc:td.ptype_loc name lds
  | _ -> Utils.unsupported_error name

let signature_item_of_type_decl ~loc:_ (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.signature_item =
  let name = td.ptype_name in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      sig_item_of_core_type name ct
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record lds; _ } ->
      (* type t = {l: T; ...} *)
      sig_item_of_record ~loc:td.ptype_loc name lds
  | _ -> Utils.unsupported_error name

let str_type_decl = Utils.make_type_decl_generator structure_item_of_type_decl

let sig_type_decl = Utils.make_type_decl_generator signature_item_of_type_decl

let deriver = P.Deriving.add "make" ~str_type_decl ~sig_type_decl
