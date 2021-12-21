#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the fsharp queueworker container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-queueworker-builddir)

mkdir -p "$DIR/app"

cp containers/gcp-fsharp-queueworker/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-fsharp-queueworker/gcp-run-fsharp-queueworker "$DIR/"
cp -R fsharp-backend/Build/out/QueueWorker/Release/net6.0/linux-x64/publish/* "$DIR/app/"

echo "${DIR}"
