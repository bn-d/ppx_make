type b = { b1 : int } [@@deriving make]

type o = { o1 : int option } [@@deriving make]

type l = { l1 : int list } [@@deriving make]

type s = { s1 : string } [@@deriving make]

(*type a = { a1 : int array }[@@deriving make]*)

(*type q = { q1 : int seq }[@@deriving make]*)

type d = { answer : int [@default 42] } [@@deriving make]

type m = { m1 : int; [@main] m2 : int [@main] } [@@deriving make]

type r = {
  r1 : int option;
  r2 : string; [@required]
  r3 : int option; [@required]
}
[@@deriving make]

type complex = {
  c1 : int;
  c2 : int option;
  c3 : int list;
  c4 : string;
  c5 : int; [@default 1024]
  c6 : int; [@required]
}
[@@deriving make]

type complex_with_main = {
  cm1 : int; [@main]
  cm2 : int option;
  cm3 : int option; [@main]
}
[@@deriving make]

let basic _ = OUnit2.assert_equal { b1 = 32 } (make_b ~b1:32 ())

let option_ _ =
  OUnit2.assert_equal { o1 = None } (make_o ());
  OUnit2.assert_equal { o1 = Some 2718 } (make_o ~o1:2718 ())

let list_ _ =
  OUnit2.assert_equal { l1 = [] } (make_l ());
  OUnit2.assert_equal { l1 = [ 1; 1; 2; 3 ] } (make_l ~l1:[ 1; 1; 2; 3 ])

let string_ _ =
  OUnit2.assert_equal { s1 = "" } (make_s ());
  OUnit2.assert_equal { s1 = "foo" } (make_s ~s1:"foo" ())

let default _ =
  OUnit2.assert_equal { answer = 42 } (make_f ());
  OUnit2.assert_equal { answer = 420 } (make_f ~answer:420 ())

let main _ = OUnit2.assert_equal { m1 = 127; m2 = 8128 } (make_m 127 8128)

let required _ =
  OUnit2.assert_equal
    { r1 = None; r2 = "bar"; r3 = Some 1729 }
    (make_r ~r2:"bar" ~r3:(Some 1729))

let complex_1 _ = ()
(* TODO *)

let complex_2 _ = ()
(* TODO *)

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
         "main" >:: main;
         "complex_1" >:: complex_1;
         "complex_2" >:: complex_2;
       ]
