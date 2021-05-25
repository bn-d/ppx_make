(*type tuple_v = Baisc of int
| T_complex of (int, int option, int list, string, int[@default 42], string[@required])
| T_complex_with_main of (int[@main], int option, int option[@main])[@@deriving make]*)

type record_v =
  | R_basic of { b1 : int }
  | R_complex of {
      c1 : int;
      c2 : int option;
      c3 : int list;
      c4 : string;
      c5 : int; [@default 1024]
      c6 : string; [@required]
    }
  | R_complex_with_main of {
      cm1 : int; [@main]
      cm2 : int option;
      cm3 : int option; [@main]
    }
[@@deriving_inline make]

let _ = fun (_ : record_v) -> ()

let (make_r_basic_of_record_v : b1:int -> unit -> record_v) =
 fun ~b1 _ -> R_basic { b1 }

let _ = make_r_basic_of_record_v

let (make_r_complex_of_record_v :
      c1:int ->
      ?c2:int ->
      ?c3:int list ->
      ?c4:string ->
      ?c5:int ->
      c6:string ->
      unit ->
      record_v) =
 fun ~c1 ?c2 ?(c3 = []) ?(c4 = "") ?(c5 = 1024) ~c6 _ ->
  R_complex { c1; c2; c3; c4; c5; c6 }

let _ = make_r_complex_of_record_v

let (make_r_complex_with_main_of_record_v :
      ?cm2:int -> int -> int option -> record_v) =
 fun ?cm2 cm1 cm3 -> R_complex_with_main { cm1; cm2; cm3 }

let _ = make_r_complex_with_main_of_record_v

[@@@end]
