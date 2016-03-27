#!/bin/bash


ocamlbuild -use-ocamlfind -Is src,src/common,src/cli,src/kernel,src/lib -tag thread -package unix -package threads -package ledit -Is src src/cartesian.native
