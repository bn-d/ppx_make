open Variant_types

let tuple_basic _ = failwith ""

let tuple_complex_1 _ = failwith ""

let tuple_complex_2 _ = failwith ""

let record_basic _ =
  OUnit2.assert_equal (R_basic { b1 = 32 }) (make_r_basic_of_record_v ~b1:32 ())

let record_complex_1 _ =
  OUnit2.assert_equal
    (R_complex { c1 = 32; c2 = None; c3 = []; c4 = ""; c5 = 1024; c6 = "Z06" })
    (make_r_complex_of_record_v ~c1:32 ~c6:"Z06" ());
  OUnit2.assert_equal
    (R_complex
       {
         c1 = 32;
         c2 = Some 64;
         c3 = [ 128; 256; 512 ];
         c4 = "boom";
         c5 = 0;
         c6 = "Z06";
       })
    (make_r_complex_of_record_v ~c1:32 ~c2:64 ~c3:[ 128; 256; 512 ] ~c4:"boom"
       ~c5:0 ~c6:"Z06" ())

let record_complex_2 _ =
  OUnit2.assert_equal
    (R_complex_with_main { cm1 = 8; cm2 = None; cm3 = None })
    (make_r_complex_with_main_of_record_v 8 None);
  OUnit2.assert_equal
    (R_complex_with_main { cm1 = 8; cm2 = Some 27; cm3 = Some 64 })
    (make_r_complex_with_main_of_record_v ~cm2:27 8 (Some 64))

let suite =
  let open OUnit2 in
  "variant"
  >::: [
         "tuple_basic" >:: tuple_basic;
         "tuple_complex_1" >:: tuple_complex_1;
         "tuple_complex_2" >:: tuple_complex_2;
         "record_basic" >:: record_basic;
         "record_complex_1" >:: record_complex_1;
         "record_complex_2" >:: record_complex_2;
       ]
