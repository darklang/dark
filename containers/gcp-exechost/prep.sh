#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the fsharp exechost container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-exechost-builddir)

cp -R backend/templates "$DIR/"
cp -R scripts "$DIR/"

mkdir -p "$DIR/app"

cp containers/gcp-exechost/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-exechost/sleep.sh "$DIR/"
cp -R fsharp-backend/Build/out/ExecHost/Release/net6.0/linux-x64/publish/* "$DIR/app/"

echo "${DIR}"
