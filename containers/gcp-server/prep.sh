#!/usr/bin/env bash
. ./scripts/devcontainer/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the main server container. Part of gcp-build-containers.

DIR=$(mktemp -d --suffix _gcp-builddir)

./containers/ocaml-container-prep.sh "${DIR}"

cp containers/gcp-server/Dockerfile "$DIR/Dockerfile"
cp containers/gcp-server/gcp-run-server "$DIR/"
cp _build/default/backend/bin/server.exe "$DIR/bin/"

# This also goes into the server container, so that if we need to run it, it's
# there in an env with access to production postgres
cp _build/default/backend/bin/emergency_login_script.exe "$DIR/bin/"

echo "${DIR}"
