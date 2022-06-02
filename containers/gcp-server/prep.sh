#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the main server container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-builddir)

./containers/ocaml-container-prep.sh "${DIR}"

cp containers/gcp-server/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-server/gcp-run-server "$DIR/"
cp _build/default/backend/bin/server.exe "$DIR/bin/"

echo "${DIR}"
