#! /usr/bin/env bash

eval '$(opam config env)'
make astenv
python main.py
