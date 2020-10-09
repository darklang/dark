#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep the tunnel container. Part of gcp-build-containers.

# It's all in the dockerfile, nothing to do.

echo containers/tunnel