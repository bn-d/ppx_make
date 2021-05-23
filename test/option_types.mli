type a = int option [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_a : ?value:int -> unit -> a
end
[@@ocaml.doc "@inline"]

[@@@end]
