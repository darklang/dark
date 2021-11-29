#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the fsharp exechost container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-exechost-builddir)

cp -R scripts "$DIR/"

cp -R backend/migrations/ "$DIR/migrations/"

cp containers/gcp-exechost/Dockerfile "$DIR/Dockerfile"

cp containers/gcp-exechost/sleep.sh "$DIR/"
chmod +x "$DIR/sleep.sh"

mkdir -p "$DIR/app"
cp -R fsharp-backend/Build/out/ExecHost/Release/net6.0/linux-x64/publish/* "$DIR/app/"

echo "${DIR}"