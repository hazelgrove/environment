#!/usr/bin/env bash

eval "$(opam env)"
. /root/.cache/pypoetry/virtualenvs/hazelnut-K3BlsyQa-py3.8/bin/activate
make astenv
echo main.py --log-name "$1" --save-dir "$2" "${@:3}"
CUBLAS_WORKSPACE_CONFIG=:4096:8 python3 main.py --log-name "$1" --save-dir "$2" "${@:3}"
# python test.py
