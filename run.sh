#! /usr/bin/env bash
name=$1
mount_dir="/RL_env/save"

if ! [ -z "$4" ]
then
    gpus="\"device=$4\""
else 
    gpus="all"
fi
echo "one:$1  two:$2 three:$3 four:$4 five:$5"
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
	"$name" "${@:3:1}" "$mount_dir"
docker logs -f "$name"
