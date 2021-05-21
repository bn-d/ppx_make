type a = int option [@@deriving make]

let none _ = OUnit2.assert_equal None @@ make_a ()

let some _ = OUnit2.assert_equal (Some 1) @@ make_a ~value:1 ()

let suite =
  let open OUnit2 in
  "option" >::: [ "none" >:: none; "some" >:: some ]
