#!/usr/bin/env bash

eval "$(opam config env)"
make ocamltest
