module A = Ppxlib.Ast
module P = Ppxlib

module Utils = Ppx_make_utils

let structure_item_of_core_type ~loc _tname (ct : A.core_type)
  : A.structure_item =
    match ct.ptyp_desc with
    | Ptyp_constr ({txt=Lident "option";_}, [_in_ct]) ->
        failwith "option not impl"
    | _ -> Utils.unsupported_error ~loc

let signature_item_of_core_type ~loc _tname (ct : A.core_type)
  : A.signature_item =
    match ct.ptyp_desc with
    | Ptyp_constr ({txt=Lident "option";_}, [_in_ct]) ->
        failwith "option not impl"
    | _ -> Utils.unsupported_error ~loc

let structure_items_of_type_decl ~loc (_rec_flag : A.rec_flag)
  (td : A.type_declaration) : A.structure_item list =
    let name = Utils.get_type_name td in
    match td with
    | {ptype_kind=Ptype_abstract; ptype_manifest=Some ct;_} ->
        (* type t = T0 *)
        [structure_item_of_core_type ~loc name ct]
    | {ptype_kind=Ptype_variant _;_} ->
        failwith "variant not impl"
    | {ptype_kind=Ptype_record _;_} ->
        failwith "record not impl"
    | _ -> Utils.unsupported_error ~loc

let signature_items_of_type_decl ~loc (_rec_flag : A.rec_flag)
  (td : A.type_declaration) : A.signature_item list =
    let name = Utils.get_type_name td in
    match td with
    | {ptype_kind=Ptype_abstract; ptype_manifest=Some ct;_} ->
        (* type t = T0 *)
        [signature_item_of_core_type ~loc name ct]
    | {ptype_kind=Ptype_variant _;_} ->
        failwith "variant not impl"
    | {ptype_kind=Ptype_record _;_} ->
        failwith "record not impl"
    | _ -> P.Location.raise_errorf ~loc
      "ppx_make cannot derive this type"

let generate_str ~ctxt (rec_flag, type_declarations) =
  let loc = P.Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map @@ structure_items_of_type_decl ~loc rec_flag
  |> List.concat

let generate_sig ~ctxt (rec_flag, type_declarations) =
  let loc = P.Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map @@ signature_items_of_type_decl ~loc rec_flag
  |> List.concat

let str_type_decl =
    P.Deriving.Generator.V2.make_noarg generate_str

let sig_type_decl =
    P.Deriving.Generator.V2.make_noarg generate_sig

let deriver =
    P.Deriving.add "make" ~str_type_decl ~sig_type_decl
