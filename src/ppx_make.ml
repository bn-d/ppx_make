module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper
module Utils = Ppx_make_utils

let value_binding_of_core_type tname (ct : P.core_type) : P.value_binding =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          let name = Ast_helper.Pat.var (Utils.gen_make_name ~loc tname) in
          let fun_ct =
            let open P in
            [%type: ?value:[%t in_ct] -> unit -> [%t ct]]
          in
          let fun_pat = Ast_helper.Pat.constraint_ name fun_ct in
          (* TODO add default *)
          let expr =
            let open P in
            [%expr fun ?value () -> value]
          in
          Ast_helper.Vb.mk fun_pat expr
      | _ -> Utils.unsupported_error ~loc)

let signature_item_of_core_type _tname (ct : P.core_type) : P.signature_item =
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ _in_ct ]) ->
      failwith "option not impl"
  | _ -> Utils.unsupported_error ~loc

let structure_items_of_type_decl ~loc (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.structure_item =
  let name = Utils.get_type_name td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ value_binding_of_core_type name ct ]
      |> Ast_helper.Str.value P.Nonrecursive
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record _; _ } -> failwith "record not impl"
  | _ -> Utils.unsupported_error ~loc

let signature_items_of_type_decl ~loc (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.signature_item =
  let name = Utils.get_type_name td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      signature_item_of_core_type name ct
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record _; _ } -> failwith "record not impl"
  | _ -> Utils.unsupported_error ~loc

let str_type_decl = Utils.make_type_decl_generator structure_items_of_type_decl

let sig_type_decl = Utils.make_type_decl_generator signature_items_of_type_decl

let deriver = P.Deriving.add "make" ~str_type_decl ~sig_type_decl
