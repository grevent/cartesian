#!/bin/bash
ocamlbuild -use-ocamlfind -Is src/kernel,src/coreLib,src/standardLib -package unix -Is src src/cartesianCLI.native
