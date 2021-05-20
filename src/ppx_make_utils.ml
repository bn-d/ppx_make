module P = Ppxlib

let unsupported_error ~loc =
  P.Location.raise_errorf ~loc "ppx_make cannot derive this type"

let make_type_decl_generator f =
  P.Deriving.Generator.V2.make_noarg @@ fun ~ctxt (rec_flag, tds) ->
  let loc = P.Expansion_context.Deriver.derived_item_loc ctxt in
  tds |> List.map @@ f ~loc rec_flag |> List.concat

let get_type_name _type_decl = ""
