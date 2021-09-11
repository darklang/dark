#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the fsharp api server container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-apiserver-builddir)

cp -R backend/templates "$DIR/"
cp -R scripts "$DIR/"

mkdir -p "$DIR/app"

cp containers/gcp-fsharp-apiserver/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-fsharp-apiserver/gcp-run-fsharp-apiserver "$DIR/"
cp -R fsharp-backend/Build/out/ApiServer/Release/net6.0/linux-x64/publish/* "$DIR/app/"

echo "${DIR}"
