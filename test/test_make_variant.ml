let test _ = ()

let suite =
  let open OUnit2 in
  "variant" >::: [ "test" >:: test ]
