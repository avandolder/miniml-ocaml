# MiniML

MiniML (pronounced "minimal") is a small ML-like programming language.
The current implementation is written in OCaml, and will (eventually) compile to Lua.

The language and implementation are heavily inspired by the
lambda-calculus interpeter built in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/).

To parse and print out a sample program, use `opam` to install `ocaml` and `dune`,
then run `dune exec ./src/miniml.exe` in the main directory.
