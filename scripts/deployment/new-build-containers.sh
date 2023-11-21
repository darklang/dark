#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

docker build . -t darkbaseservice:latest -f containers/base-service-Dockerfile
DOCKER_BUILDKIT=0 docker build . -t darkfsharpservice:latest -f containers/fsharp-service-Dockerfile --pull=false
DOCKER_BUILDKIT=0 docker buildx build . -t bwdserver:latest -f containers/bwdserver/Dockerfile --pull=false
