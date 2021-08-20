type 'a poly_val = V_value of 'a [@@deriving make]

type ('a, 'b) poly_rec = { r_a : 'a; r_b : 'b; r_int : int } [@@deriving make]
