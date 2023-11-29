#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

docker build . -t darkbaseservice:latest -f containers/base-service-Dockerfile --load
docker build . -t darkfsharpservice:latest -f containers/fsharp-service-Dockerfile --load --pull=false
docker build . -t bwdserver:latest -f containers/bwdserver/Dockerfile --load --pull=false
docker build . -t prodexec:latest -f containers/prodexec/Dockerfile --load
