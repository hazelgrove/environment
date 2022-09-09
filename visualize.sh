#! /usr/bin/env bash

name=$1
mount_dir=$2
docker build -t "$name" .
docker run --rm -it \
        --env-file .env \
        --shm-size=10.24gb \
        --name "$name" \
        --gpus all \
    --network="host" \
        -h="$(hostname -s)" \
        -e TERM=xterm-256color \
    -v rl_checkpoint:"$mount_dir" \
    --entrypoint /RL_env/visualize_entrypoint.sh \
    "$name" "${@:3}" "$mount_dir"
    