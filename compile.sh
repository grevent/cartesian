#!/bin/bash
ocamlbuild -use-ocamlfind -Is src/kernel,src/coreLib,src/standardLib -tag thread -package unix -Is src src/cartesianCLI.native
