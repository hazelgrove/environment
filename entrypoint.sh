#! /usr/bin/env bash

eval '$(opam env)'
make astenv
python test.py
