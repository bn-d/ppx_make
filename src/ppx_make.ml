module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper
module Utils = Ppx_make_utils

let str_item_of_core_type name (ct : P.core_type) : P.structure_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          (* T option *)
          let fun_pat = Ast_helper.Pat.var @@ Utils.gen_make_name name in
          let return_ct = Utils.core_type_of_name name in
          (* TODO add default *)
          let open P in
          [%stri
            let ([%p fun_pat] : ?value:[%t in_ct] -> unit -> [%t return_ct]) =
             fun ?value () -> value]
      | _ -> Utils.unsupported_error name)

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

let str_item_of_record ~loc name (lds : P.label_declaration list) :
    P.structure_item =
  Ast_helper.with_default_loc loc (fun () ->
      let fun_pat = Ast_helper.Pat.var @@ Utils.gen_make_name name in
      (*let expr = lds
        |> List.fold_left
        |> fun () -> expr
        in*)
      ignore fun_pat;
      ignore lds;
      failwith "")

let sig_item_of_record ~loc name (lds : P.label_declaration list) :
    P.signature_item =
  Ast_helper.with_default_loc loc (fun () ->
      let fun_name = Utils.gen_make_name name in
      lds
      |> List.fold_left
           (fun (label_list, main_list)
                P.{ pld_name; pld_type; pld_attributes; _ } ->
             let attr_type : Utils.attr_type =
               Utils.get_attributes pld_attributes
             in
             let optional = Utils.is_core_type_optional pld_type in
             match (attr_type, optional) with
             | Main, _ -> (label_list, pld_type :: main_list)
             | Default _, _ | No_attr, true ->
                 let ct = Utils.strip_option pld_type in
                 ((P.Optional pld_name.txt, ct) :: label_list, main_list)
             | _, _ ->
                 ( (P.Asttypes.Labelled pld_name.txt, pld_type) :: label_list,
                   main_list ))
           ([], [])
      |> (fun (label_list, main_list) ->
           let main_list =
             match main_list with
             | [] -> [ Utils.unit_core_type ~loc ]
             | _ -> main_list
           in
           name |> Utils.core_type_of_name |> fun ct ->
           List.fold_left
             (fun acc cur -> Ast_helper.Typ.arrow P.Nolabel cur acc)
             ct main_list
           |> fun ct ->
           List.fold_left
             (fun acc (arg_label, cur) ->
               Ast_helper.Typ.arrow arg_label cur acc)
             ct label_list)
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
