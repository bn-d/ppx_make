type b = int * int [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_b : v1:int -> v2:int -> unit -> b
end
[@@ocaml.doc "@inline"]

[@@@end]

type o = int option * int option [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_o : ?v1:int -> ?v2:int -> unit -> o
end
[@@ocaml.doc "@inline"]

[@@@end]

type l = int list * int list [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_l : ?v1:int list -> ?v2:int list -> unit -> l
end
[@@ocaml.doc "@inline"]

[@@@end]

type s = string * string [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_s : ?v1:string -> ?v2:string -> unit -> s
end
[@@ocaml.doc "@inline"]

[@@@end]

type d = (int[@default 42]) * (int[@default 420]) [@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_d : ?v1:(int[@default 42]) -> ?v2:(int[@default 420]) -> unit -> d
end
[@@ocaml.doc "@inline"]

[@@@end]

type r = int option * (string[@required]) * (int option[@required])
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_r :
    ?v1:int -> v2:(string[@required]) -> v3:(int option[@required]) -> unit -> r
end
[@@ocaml.doc "@inline"]

[@@@end]

type complex =
  int
  * int option
  * int list
  * string
  * (int[@default 1024])
  * (string[@required])
[@@deriving_inline make]

include sig
  [@@@ocaml.warning "-32"]

  val make_complex :
    v1:int ->
    ?v2:int ->
    ?v3:int list ->
    ?v4:string ->
    ?v5:(int[@default 1024]) ->
    v6:(string[@required]) ->
    unit ->
    complex
end
[@@ocaml.doc "@inline"]

[@@@end]
