#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the fsharp cronchecker container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-cronchecker-builddir)

mkdir -p "$DIR/app"

cp containers/gcp-fsharp-cronchecker/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-fsharp-cronchecker/gcp-run-fsharp-cronchecker "$DIR/"
cp -R fsharp-backend/Build/out/CronChecker/Release/net6.0/linux-x64/publish/* "$DIR/app/"

echo "${DIR}"
