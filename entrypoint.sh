#! /usr/bin/env bash

eval "$(opam env)"
. /root/.cache/pypoetry/virtualenvs/hazelnut-K3BlsyQa-py3.8/bin/activate
make astenv
python main.py --log-name "$1" --save-dir "$2" # --sweep
