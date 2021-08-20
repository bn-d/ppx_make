type 'a poly_val = V_value of 'a [@@deriving_inline make]

include
  sig
    [@@@ocaml.warning "-32"]
    val make_v_value_of_poly_val : v0:'a -> unit -> poly_val
  end[@@ocaml.doc "@inline"]
[@@@end]

type ('a, 'b) poly_rec = { r_a : 'a; r_b : 'b; r_int : int }
[@@deriving_inline make]

include
  sig
    [@@@ocaml.warning "-32"]
    val make_poly_rec : r_a:'a -> r_b:'b -> r_int:int -> unit -> poly_rec
  end[@@ocaml.doc "@inline"]
[@@@end]
