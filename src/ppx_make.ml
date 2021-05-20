module A = Ppxlib.Ast
module P = Ppxlib
module Utils = Ppx_make_utils

let structure_item_of_core_type ~loc _tname (ct : A.core_type) :
    A.structure_item =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ _in_ct ]) ->
      failwith "option not impl"
  | _ -> Utils.unsupported_error ~loc

let signature_item_of_core_type ~loc _tname (ct : A.core_type) :
    A.signature_item =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ _in_ct ]) ->
      failwith "option not impl"
  | _ -> Utils.unsupported_error ~loc

let structure_items_of_type_decl ~loc (_rec_flag : A.rec_flag)
    (td : A.type_declaration) : A.structure_item list =
  let name = Utils.get_type_name td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ structure_item_of_core_type ~loc name ct ]
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record _; _ } -> failwith "record not impl"
  | _ -> Utils.unsupported_error ~loc

let signature_items_of_type_decl ~loc (_rec_flag : A.rec_flag)
    (td : A.type_declaration) : A.signature_item list =
  let name = Utils.get_type_name td in
  match td with
  | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      (* type t = T0 *)
      [ signature_item_of_core_type ~loc name ct ]
  | { ptype_kind = Ptype_variant _; _ } -> failwith "variant not impl"
  | { ptype_kind = Ptype_record _; _ } -> failwith "record not impl"
  | _ -> P.Location.raise_errorf ~loc "ppx_make cannot derive this type"

let str_type_decl = Utils.make_type_decl_generator structure_items_of_type_decl

let sig_type_decl = Utils.make_type_decl_generator signature_items_of_type_decl

let deriver = P.Deriving.add "make" ~str_type_decl ~sig_type_decl
