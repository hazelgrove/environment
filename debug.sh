#! /usr/bin/env bash

docker build -t debug .
docker run --rm -it \
	--env-file .env \
	--shm-size=10.24gb \
	--name debug \
	--gpus all \
	--network="host" \
	-h="$(hostname -s)" \
	-e TERM=xterm-256color \
    --entrypoint /RL_env/debug_entrypoint.sh \
	debug "${@:1}"