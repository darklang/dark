#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the main server container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-builddir)

./containers/ocaml-container-prep.sh "${DIR}"

cp containers/legacyserver/Dockerfile "$DIR/Dockerfile"
cp containers/legacyserver/gcp-run-legacyserver "$DIR/"
cp _build/default/backend/bin/legacyserver.exe "$DIR/bin/"

echo "${DIR}"
