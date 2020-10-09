#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the stroller container. Part of gcp-build-containers.

if [[ ! -v CI ]]; then
    # If not in CI, we may not have a release build yet - build it
    scripts/support/build-rust-release containers/stroller
fi

DIR=$(mktemp -d --suffix _gcp-stroller-builddir)

mkdir -p "$DIR/bin"
cp containers/stroller/Dockerfile "$DIR/Dockerfile"
cp containers/stroller/target/release/dark-stroller "$DIR/bin/"
cp scripts/support/check-linked-libs "$DIR/"

echo "${DIR}"