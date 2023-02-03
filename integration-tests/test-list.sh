#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Get list of integration tests

set -euo pipefail

cat integration-tests/tests.ts \
  | grep "^  test(" integration-tests/tests.ts \
  | sed 's/.*"\(.*\)".*/\1/'
