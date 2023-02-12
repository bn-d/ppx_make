# [@@deriving make]
[![OCaml](https://img.shields.io/badge/-OCaml-EC6813?logo=ocaml&labelColor=grey)](#)
[![CI](https://github.com/bn-d/ppx_make/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/bn-d/ppx_make/actions/workflows/build.yml)
[![GitHub release status](https://img.shields.io/github/v/release/bn-d/ppx_make)](https://github.com/bn-d/ppx_make/releases)

ppxlib based make deriver

## Installation
`ppx_make` can be installed via [OCaml Package Manager](https://opam.ocaml.org/packages/ppx_make/).
```sh
$ opam install ppx_make
```

## Usage
Please see the [documentation](https://boni.ng/ppx_make/ppx_make/index.html).

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
