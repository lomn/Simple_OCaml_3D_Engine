# Simple OCaml 3D Engine

## Project description

This project is something I wrote while following more advanced math courses. The goal here was to write most of the tools used except for the window part, i.e I re-wrote/wrote matrix operations, file interpreter and the 3D engine in itself. This was done as a challenge and the end goal wasn't to have some complex and efficient 3D engine (bear that in mind) <u>but rather to write everything from scratch</u>.


## How does it works

In the source file there are two files that you can run on your machine directly. First one is a very small python script that converts stl files to some custom, simpler file that is understandable by the 3D engine. The second file of interest is the "main_graphic.ml" file which is where all the magic happens.

By default you can run the "ocaml main_graphic.ml" to see a quick demo of the engine (same as gif below).


## Limitations

As you can see in the gif here, the engine is slow when relatively complex shapes are used this is probably because all the operations are done by the cpu and I didn't and the code wasn't compiled when I recorded this demo.

## If you want to try it

In order to test this small engine you will need a running version of OCaml with opam and the graphics library installed.
