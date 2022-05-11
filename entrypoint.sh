#! /usr/bin/env bash

eval "$(opam env)"
. /root/.cache/pypoetry/virtualenvs/hazelnut-K3BlsyQa-py3.8/bin/activate
make astenv
# python test.py
python main.py --env-name "PongNoFrameskip-v4" --use-gae --lr 2.5e-4 --clip-param 0.1 --value-loss-coef 0.5 --num-processes 8 --num-steps 128 --num-mini-batch 4 --log-interval 1 --use-linear-lr-decay --entropy-coef 0.01
