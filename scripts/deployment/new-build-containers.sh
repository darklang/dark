#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

# We specify `docker build –-pull=false` because otherwise it looks for the
# containers we just built in the repository, even if they're available locally. As a
# result, we first need to pull the actual remote containers we need from docker.
docker pull amd64/buildpack-deps:jammy-curl

docker build . -t darkbaseservice:latest -f containers/base-service-Dockerfile --load --pull=false
docker build . -t darkfsharpservice:latest -f containers/fsharp-service-Dockerfile --load --pull=false
docker build . -t bwdserver:latest -f containers/bwdserver/Dockerfile --load --pull=false
docker build . -t prodexec:latest -f containers/prodexec/Dockerfile --load --pull=false
