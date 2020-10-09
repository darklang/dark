#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the queueworker container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-qw-builddir)

./containers/ocaml-container-prep.sh "${DIR}"

cp containers/gcp-queueworker/gcp-run-queueworker "$DIR/"
cp containers/gcp-queueworker/Dockerfile "$DIR/"
cp _build/default/backend/bin/queue_worker.exe "$DIR/bin/"

echo "${DIR}"