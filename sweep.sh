#! /usr/bin/env bash

# usage: ./sweep.sh DOCKER_CONTAINER_NAME LOG_NAME [Gpu specification]
name=$1
mount_dir="/RL_env/save"
log_name=$2

if ! [ -z "$3" ]
then
    gpus="\"device=$3\""
else 
    gpus="all"
fi
echo "using gpus: $gpus"
docker build -t "$name" .

docker run --rm -it -d \
	--env-file .env \
	--shm-size=10.24gb \
	--name "$name" \
	--gpus $gpus \
	--network="host" \
	-h="$(hostname -s)" \
	-e TERM=xterm-256color \
	-v rl_checkpoint:"$mount_dir" \
	"$name" "$log_name" "$mount_dir" --sweep
docker logs -f "$name"
