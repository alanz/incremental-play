# incremental-play

Playing with incremental parsing and later compiler passes

First pass will be to build a haskell version of the approach from "Efficient
and Flexible Incremental Parsing",
https://pdfs.semanticscholar.org/4d22/fab95c78b3c23fa9dff88fb82976edc213c2.pdf

This is also implemented in https://github.com/tree-sitter/tree-sitter

## To experiment

Install the custom version of `happy` from https://github.com/alanz/happy, using
the `incremental` branch.

In this directory

    make

Load `app/Main.hs` into ghci and run it

