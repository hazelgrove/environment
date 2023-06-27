#! /usr/bin/env bash

name=$1
mount_dir=$2

gpus="all"
if ! [ -z "$5" ]
then
    gpus="\"device=$5\""
fi
echo "using gpus: $gpus"


docker build -t "$name" .
docker run --rm -it \
	--env-file .env \
	--shm-size=10.24gb \
	--name "$name" \
	--gpus $gpus \
	--network="host" \
	-h="$(hostname -s)" \
	-e TERM=xterm-256color \
	-v rl_checkpoint:"$mount_dir" \
	--entrypoint /RL_env/visualize_entrypoint.sh \
	"$name" "${@:3:2}" "$mount_dir"
