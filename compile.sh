#!/bin/bash


ocamlbuild -use-ocamlfind -Is src,src/common,src/cli,src/compiler,src/lib -tag thread -package xmlm -package unix -package threads -package ledit -Is src src/cartesian.native
