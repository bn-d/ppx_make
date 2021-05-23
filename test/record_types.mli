type b = { b1 : int } [@@deriving_inline make]

[@@@end]

type o = { o1 : int option } [@@deriving_inline make]

[@@@end]

type l = { l1 : int list } [@@deriving_inline make]

[@@@end]

type s = { s1 : string } [@@deriving_inline make]

[@@@end]

(*type a = { a1 : int array }[@@deriving_inline make][@@@end]*)

(*type q = { q1 : int seq }[@@deriving_inline make][@@@end]*)

type d = { answer : int [@default 42] } [@@deriving_inline make]

[@@@end]

type m = { m1 : int; [@main] m2 : int [@main] } [@@deriving_inline make]

[@@@end]

type r = {
  r1 : int option;
  r2 : string; [@required]
  r3 : int option; [@required]
}
[@@deriving_inline make]

[@@@end]

type complex = {
  c1 : int;
  c2 : int option;
  c3 : int list;
  c4 : string;
  c5 : int; [@default 1024]
  c6 : int; [@required]
}
[@@deriving_inline make]

[@@@end]

type complex_with_main = {
  cm1 : int; [@main]
  cm2 : int option;
  cm3 : int option; [@main]
}
[@@deriving_inline make]

[@@@end]
