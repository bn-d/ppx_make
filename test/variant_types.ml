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
[@@deriving make]
