#!/bin/bash


# ocamlbuild -use-ocamlfind -Is src/kernel,src/interface,src/kernel,src/coreLib,src/standardLib,src/runtime,src/runtime/expressions,src/runtime/functions,src/runtime/actions,src/runtime/objects,src/runtime/patterns,src/runtime/prototypes -tag thread -package unix -package threads -Is src src/cartesianCLI.native
ocamlbuild -use-ocamlfind -Is src,src/common,src/parse,src/compile,src/run,src/analyse -tag thread -package unix -package threads -Is src src/cartesianCLI.native
