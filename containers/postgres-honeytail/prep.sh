#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "${@}"

set -euo pipefail

# Prep postgres-honeytail. Part of gcp-build-containers

# It's all in the dockerfile, nothing to do.

echo containers/postgres-honeytail