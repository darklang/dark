#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the fsharp api server container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-bwdserver-builddir)

cp -R backend/templates "$DIR/"
cp -R scripts "$DIR/"

mkdir -p "$DIR/app"

cp containers/gcp-fsharp-bwdserver/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-fsharp-bwdserver/gcp-run-fsharp-bwdserver "$DIR/"
cp -R fsharp-backend/Build/out/BwdServer/Release/net6.0/linux-x64/publish/* "$DIR/app/"

echo "${DIR}"
