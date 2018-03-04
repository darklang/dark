#!/bin/bash

set -euo pipefail

PATTERN=${@}
PATTERN=${PATTERN:-".*"} # default to .*

# Slowing this down massively slows down the test suite. If needed, we
# can change this on an individual action:
# https://devexpress.github.io/testcafe/documentation/test-api/actions/action-options.html#basic-action-options
SPEED=1

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
