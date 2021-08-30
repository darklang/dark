#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the cronchecker container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-cron-builddir)

./containers/ocaml-container-prep.sh "${DIR}"

cp containers/gcp-cronchecker/gcp-run-cronchecker "$DIR/"
cp containers/gcp-cronchecker/Dockerfile "$DIR/"
cp _build/default/backend/bin/cron_checker.exe "$DIR/bin/"

echo "${DIR}"
