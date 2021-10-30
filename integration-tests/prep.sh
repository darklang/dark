#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Prep the integration tests

set -euo pipefail

# Set up test reporters for CircleCI
TEST_DIR="${DARK_CONFIG_RUNDIR}/integration-tests"


echo "Clearing old test files"
rm -Rf "${TEST_DIR}"
mkdir -p "${TEST_DIR}"
rm -f ${DARK_CONFIG_RUNDIR}/test_results/integration_tests.*

./integration-tests/clear-db.sh
