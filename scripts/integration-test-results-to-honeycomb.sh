#!/usr/bin/env bash
# Disable useless-use-of-cat
# shellcheck disable=SC2002
. ./scripts/support/assert-in-container "$0" "$@"

set -euo pipefail

# We didn't keep the honeycomb buildevent orb, but the api key is still in place
# for it
export HONEYCOMB_API_KEY=$BUILDEVENT_APIKEY

test_results=rundir/test_results/integration_tests.json
# This format is RFC3339 down to hundredths of a second, which honeycomb
# accepts (despite docs specifying either RFC3339 or RFC3339 with nanoseconds)
timestamp="$(cat $test_results | jq -r '.endTime')"

cat $test_results \
    | scripts/support/process-integration-test-results.sh \
    > rundir/test_results/integration_tests_for_honeycomb.json


./honeytail --parser=json \
          --writekey="${HONEYCOMB_API_KEY}" \
          --dataset="integration-tests" \
          --add_field="timestamp=${timestamp}" \
          --backfill \
          --file=rundir/test_results/integration_tests_for_honeycomb.json
