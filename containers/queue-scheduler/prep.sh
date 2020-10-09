#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the queue-scheduler container. Part of gcp-build-containers.

if [[ ! -v CI ]]; then
    # If not in CI, we may not have a release build yet - build it
    scripts/support/build-rust-release containers/queue-scheduler
fi

DIR=$(mktemp -d --suffix _gcp-scheduler-builddir)

mkdir -p "$DIR/bin"
cp containers/queue-scheduler/Dockerfile "$DIR/Dockerfile"
cp containers/queue-scheduler/target/release/dark-queue-scheduler "$DIR/bin/"
cp scripts/support/check-linked-libs "$DIR/"

echo "${DIR}"