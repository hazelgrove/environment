#! /usr/bin/env bash

eval "$(opam env)"
. /root/.cache/pypoetry/virtualenvs/hazelnut-K3BlsyQa-py3.8/bin/activate
make astenv
python main.py --log-name "PL with maximum 1 timestep" --gnn
# python test.py
