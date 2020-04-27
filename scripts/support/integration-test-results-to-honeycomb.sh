#!/usr/bin/env bash
# Disable useless-use-of-cat
# shellcheck disable=SC2002
. ./scripts/support/assert-in-container "$0" "$@"

# Collects the results of integration tests and sends to honeycomb. See
# scripts/support/process-integration-test-results.sh for what gets sent.

set -euo pipefail

# We didn't keep the honeycomb buildevent orb, but the api key is still in place
# for it
export HONEYCOMB_API_KEY=$BUILDEVENT_APIKEY

# Install honeytail here, since installing it in the Dockerfile just leaves me
# with "honeytail: command not found"
if [[ ! -f honeytail ]]; then
    wget -q -O honeytail https://honeycomb.io/download/honeytail/linux/1.762 && \
      echo '00e24441316c7ae24665b1aaea4cbb77e2ee52c83397bf67d70f3ffe14a1e341  honeytail' | sha256sum -c && \
      chmod 755 ./honeytail
fi

test_results=rundir/test_results/integration_tests.json
# This format is RFC3339 down to hundredths of a second, which honeycomb
# accepts (despite docs specifying either RFC3339 or RFC3339 with nanoseconds)
timestamp="$(cat $test_results | jq -r '.endTime')"

cat $test_results \
    | scripts/support/process-integration-test-results.sh \
    | ./honeytail --parser=json \
                  --writekey="${HONEYCOMB_API_KEY}" \
                  --dataset="integration-tests" \
                  --add_field="timestamp=${timestamp}" \
                  --backfill \
                  --file=-
