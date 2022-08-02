#! /usr/bin/env bash

name=rl
docker build -t "$name" .
docker run --rm -it -d \
        --env-file .env \
        --shm-size=10.24gb \
        --name "$name" \
        --gpus all \
    --network="host" \
        -h="$(hostname -s)" \
        -e TERM=xterm-256color \
    "$name" "${@:1}"
docker logs -f "$name"
    