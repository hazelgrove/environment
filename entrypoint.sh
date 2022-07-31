#! /usr/bin/env bash

eval "$(opam env)"
. /root/.cache/pypoetry/virtualenvs/hazelnut-K3BlsyQa-py3.8/bin/activate
make astenv
python main.py --log-name "ALE/Breakout-v5" --gnn False
# python test.py
