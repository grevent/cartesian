#!/bin/bash
ocamlbuild -use-ocamlfind -Is src/kernel,src/coreLib -package unix -Is src src/cartesianCLI.native
