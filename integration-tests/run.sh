#!/usr/bin/env bash

set -euo pipefail

HOST="localhost:8000"
PATTERN=".*"

for i in "$@"
do
  case "${i}" in
    --host=*)
    HOST=${1/--host=/''}
    ;;
    --pattern)
    PATTERN=${1/--pattern=/''}
    ;;
  esac
done



# Slowing this down massively slows down the test suite. If needed, we
# can change this on an individual action:
# https://devexpress.github.io/testcafe/documentation/test-api/actions/action-options.html#basic-action-options
SPEED=1

TEST_HOST=$HOST \
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 0 \
    --speed $SPEED \
    --screenshots-on-fails \
    --screenshots $DARK_CONFIG_RUN_DIR/screenshots/ \
    --concurrency 4 \
    --reporter spec,json:$DARK_CONFIG_RUN_DIR/integration_tests.json \
    --test-grep "$PATTERN" \
    "chrome:headless" \
    integration-tests/tests.js
