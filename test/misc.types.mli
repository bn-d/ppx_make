(* https://github.com/bn-d/ppx_make/issues/12 *)
type a = { i : int } [@@deriving make, show]
and b = int
