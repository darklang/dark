#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

# docker build . -t dark-base-service:latest -f containers/base-service-Dockerfile
# docker build . -t dark-fsharp-service:latest -f containers/fsharp-service-Dockerfile
docker build . -t bwdserver:latest -f containers/bwdserver/Dockerfile
gcloud auth configure-docker us-central1-docker.pkg.dev/production-containers
docker tag bwdserver:latest us-central1-docker.pkg.dev/darklang-next/production-containers/bwdserver:latest
docker image push us-central1-docker.pkg.dev/darklang-next/production-containers/bwdserver:latest
