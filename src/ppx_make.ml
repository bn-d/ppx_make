module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper
module Utils = Ppx_make_utils

let str_item_of_core_type name (ct : P.core_type) : P.structure_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          let fun_pat = Ast_helper.Pat.var (Utils.gen_make_name ~loc name) in
          (* TODO add default *)
          let open P in
          [%stri
            let ([%p fun_pat] : ?value:[%t in_ct] -> unit -> [%t ct]) =
             fun ?value () -> value]
      | _ -> Utils.unsupported_error ~loc)

let sig_item_of_core_type name (ct : P.core_type) : P.signature_item =
  let loc = ct.ptyp_loc in
  Ast_helper.with_default_loc loc (fun () ->
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, [ in_ct ]) ->
          let fun_name = Utils.gen_make_name ~loc name in
          let open P in
          [%type: ?value:[%t in_ct] -> unit -> [%t ct]]
          |> Ast_helper.Val.mk fun_name |> Ast_helper.Sig.value
      | _ -> Utils.unsupported_error ~loc)

let structure_item_of_type_decl ~loc (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.structure_item =
  let name = Utils.get_type_name td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      str_item_of_core_type name ct
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record _; _ } -> failwith "record not impl"
  | _ -> Utils.unsupported_error ~loc

let signature_item_of_type_decl ~loc (_rec_flag : P.rec_flag)
    (td : P.type_declaration) : P.signature_item =
  let name = Utils.get_type_name td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      sig_item_of_core_type name ct
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record _; _ } -> failwith "record not impl"
  | _ -> Utils.unsupported_error ~loc

let str_type_decl = Utils.make_type_decl_generator structure_item_of_type_decl

let sig_type_decl = Utils.make_type_decl_generator signature_item_of_type_decl

let deriver = P.Deriving.add "make" ~str_type_decl ~sig_type_decl
