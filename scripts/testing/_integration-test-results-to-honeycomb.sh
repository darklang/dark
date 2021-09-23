#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Disable useless-use-of-cat
# shellcheck disable=SC2002

# Collects the results of integration tests and sends to honeycomb. See
# scripts/testing/_process-integration-test-results.sh for what gets sent.

set -euo pipefail

export HONEYCOMB_API_KEY=$BUILDEVENT_APIKEY

test_results=rundir/test_results/integration_tests.json
# This format is RFC3339 down to hundredths of a second, which honeycomb
# accepts (despite docs specifying either RFC3339 or RFC3339 with nanoseconds)
timestamp="$(cat $test_results | jq -r '.endTime')"

cat $test_results \
    | scripts/testing/_process-integration-test-results.sh \
    | honeytail --parser=json \
                --writekey="${HONEYCOMB_API_KEY}" \
                --dataset="integration-tests" \
                --add_field="timestamp=${timestamp}" \
                --backfill \
                --file=-
