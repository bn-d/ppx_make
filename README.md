# [@@deriving make]
[![OCaml][ocaml-badge]](#)
[![CI][ci-badge]](https://github.com/bn-d/ppx_make/actions/workflows/build.yml)
[![GitHub release status][release-badge]](https://github.com/bn-d/ppx_make/releases)
[![Coverage Status][coveralls-badge]](https://coveralls.io/github/bn-d/ppx_make?branch=main)

[ocaml-badge]: https://img.shields.io/badge/-OCaml-EC6813?logo=ocaml&labelColor=white
[ci-badge]: https://github.com/bn-d/ppx_make/actions/workflows/build.yml/badge.svg?branch=main
[release-badge]: https://img.shields.io/github/v/release/bn-d/ppx_make
[coveralls-badge]: https://coveralls.io/repos/github/bn-d/ppx_make/badge.svg?branch=main

`[@@deriving]` plugin to generate make functions.

## Installation
`ppx_make` can be installed via [OCaml Package Manager](https://opam.ocaml.org/packages/ppx_make/).
```sh
$ opam install ppx_make
```

## Usage
Please see the [documentation](https://ocaml.org/p/ppx_make/latest/doc/index.html).

## Example
```ocaml
type my_type = {
  my_field : int;
  my_option : int option;
  my_list : int list;
  my_string : string;
  my_default : int; [@default 1024]
}
[@@deriving make]

(* the deriver will automatically generate the function below *)
val make_my_type :
  my_field:int ->
  ?my_option:int ->
  ?my_list:int list ->
  ?my_string:string ->
  ?my_default:int ->
  unit ->
  my_type
```
