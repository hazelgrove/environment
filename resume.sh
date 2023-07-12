#! /usr/bin/env bash

# usage: ./run.sh container_name run_name resume_from_run_name resume_from_run_id [gpu_num]

name=$1
mount_dir="/RL_env/save"
log_name=$2
load_name=$3
load_id=$4

if ! [ -z "$5" ] # TODO: update 
then
    gpus="\"device=$5\""
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
	"$name" "$log_name" "$mount_dir" -r --resume_id "$load_id" --resume_name "$load_name" 
docker logs -f "$name"
