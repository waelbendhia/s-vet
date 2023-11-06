#!/usr/bin/env bash

set -o allexport
source .env
set +o allexport

export NODE_OPTIONS=--openssl-legacy-provider

docker compose up db -d

prefix() {
  local prefix="$1"
  shift
  "$@" > >(sed "s/^/$prefix: /") 2> >(sed "s/^/$prefix (err): /" >&2)
}

cd s-vet/
prefix "server" watchexec -r -e hs ./build-and-exec.sh  &
SERVER_ID=$!

cd ../s-vet-web/
prefix "app" yarn install && yarn start 

wait $SERVER_ID
