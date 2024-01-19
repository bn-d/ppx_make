let test_multiple_type_decl _ =
  let open Misc_types in
  OUnit.assert_equal { i = 1 } @@ make_a ~i:1 ()

let _ =
  let open OUnit2 in
  run_test_tt_main
    ("test_make"
    >::: [
           Test_make_option.suite;
           Test_make_poly.suite;
           Test_make_record.suite;
           Test_make_tuple.suite;
           Test_make_variant.suite;
           "test_multiple_type_decl" >:: test_multiple_type_decl;
         ])
