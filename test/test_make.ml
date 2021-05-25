let _ =
  let open OUnit2 in
  run_test_tt_main
    ("test_make"
    >::: [
           Test_make_option.suite;
           Test_make_record.suite;
           (* TODO Test_make_tuple.suite;*)
           Test_make_variant.suite;
         ])
