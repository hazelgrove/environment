#! /usr/bin/env bash
name=$1
mount_dir=$2
gpus="all"
if ! [ -z "$4" ]
then
    gpus="\"device=$4\""
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
	"$name" "${@:3}" "$mount_dir"
docker logs -f "$name"
