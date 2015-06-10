#!/bin/bash


# ocamlbuild -use-ocamlfind -Is src/kernel,src/interface,src/kernel,src/coreLib,src/standardLib,src/runtime,src/runtime/expressions,src/runtime/functions,src/runtime/actions,src/runtime/objects,src/runtime/patterns,src/runtime/prototypes -tag thread -package unix -package threads -Is src src/cartesianCLI.native
ocamlbuild -use-ocamlfind -Is src/kernel,src/interfaces,src/coreLib,src/standardLib,src/kernel/expressions,src/kernel/functions,src/kernel/actions,src/kernel/objects,src/kernel/patterns,src/kernel/prototypes -tag thread -package unix -package threads -Is src src/cartesianCLI.native
