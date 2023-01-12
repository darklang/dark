#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Collects the results of integration tests and sends to honeycomb. See
# _process-integration-test-results.sh for what gets sent.

set -euo pipefail

# Disable useless-use-of-cat
# shellcheck disable=SC2002

# This format is RFC3339 down to hundredths of a second, which honeycomb
# accepts (despite docs specifying either RFC3339 or RFC3339 with nanoseconds)
timestamp=$(date)

if [[ -v HONEYCOMB_EVENT_KEY ]]; then
  cat rundir/test_results/integration_tests.json \
      | integration-tests/_process-integration-test-results.sh \
      | honeytail --parser=json \
                  --writekey="${HONEYCOMB_EVENT_KEY}" \
                  --dataset="integration-tests" \
                  --add_field="timestamp=${timestamp}" \
                  --backfill \
                  --file=-
fi