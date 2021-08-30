#!/usr/bin/env bash
. ./scripts/devcontainer/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the garbage-collector container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-gc-builddir)

./containers/ocaml-container-prep.sh "${DIR}"

cp containers/gcp-garbagecollector/gcp-run-garbagecollector "$DIR/"
cp containers/gcp-garbagecollector/Dockerfile "$DIR/"
cp _build/default/backend/bin/garbage_collector_worker.exe "$DIR/bin/"

echo "${DIR}"
