#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "${@}"

set -euo pipefail

# Part of gcp-build-containers, copies the files needed for any OCaml service.
# Called from prep.sh in OCaml containers

DIR=$1

cp scripts/linting/_check-linked-libs "$DIR/"
cp -R backend/templates "$DIR/"
cp -R scripts "$DIR/"

mkdir -p "$DIR/bin"

mkdir -p "$DIR/webroot"
cp -R backend/static "$DIR/webroot/"

mkdir -p "$DIR/migrations"
cp -R backend/migrations/* "$DIR/migrations/"
