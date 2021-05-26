open Tuple_types

let basic _ = OUnit2.assert_equal (32, 44) (make_b ~v1:32 ~v2:44 ())

let option_ _ =
  OUnit2.assert_equal (None, None) (make_o ());
  OUnit2.assert_equal (Some 2718, Some 2818) (make_o ~v1:2718 ~v2:2818 ())

let list_ _ =
  OUnit2.assert_equal ([], []) (make_l ());
  OUnit2.assert_equal
    ([ 1; 1; 2; 3 ], [ 5; 8; 11; 19 ])
    (make_l ~v1:[ 1; 1; 2; 3 ] ~v2:[ 5; 8; 11; 19 ] ())

let string_ _ =
  OUnit2.assert_equal ("", "") (make_s ());
  OUnit2.assert_equal ("foo", "bar") (make_s ~v1:"foo" ~v2:"bar" ())

let default _ =
  OUnit2.assert_equal (42, 420) (make_d ());
  OUnit2.assert_equal (420, 42) (make_d ~v1:420 ~v2:42 ())

let required _ =
  OUnit2.assert_equal (None, "cov", Some 1729)
    (make_r ~v2:"cov" ~v3:(Some 1729) ())

let complex_1 _ =
  OUnit2.assert_equal
    (32, None, [], "", 1024, "Z06")
    (make_complex ~v1:32 ~v6:"Z06" ());
  OUnit2.assert_equal
    (32, Some 64, [ 128; 256; 512 ], "boom", 0, "Z06")
    (make_complex ~v1:32 ~v2:64 ~v3:[ 128; 256; 512 ] ~v4:"boom" ~v5:0 ~v6:"Z06"
       ())

let suite =
  let open OUnit2 in
  "record"
  >::: [
         "basic" >:: basic;
         "option" >:: option_;
         "list" >:: list_;
         "string" >:: string_;
         "default" >:: default;
         "required" >:: required;
         "complex_1" >:: complex_1;
       ]
