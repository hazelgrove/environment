#! /usr/bin/env bash

docker-compose down
docker-compose build --pull
docker-compose --env-file .env up --remove-orphans --force-recreate
