open Poly_types

let poly_val _ =
  OUnit2.assert_equal (V_value 123) (make_v_value_of_poly_val ~v0:123 ())

let suite =
  let open OUnit2 in
  "polymorphic" >::: [ "poly_val" >:: poly_val]
