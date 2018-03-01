#!/bin/bash

set -euo pipefail

# clean test files (reset repo files, delete others)
git checkout -- $(git ls-files server/appdata/test_*.dark)
rm -f $(git ls-files server/appdata/test_*.dark -o)

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
  --screenshots integration-tests/screenshots/ \
  --concurrency 4 \
  --reporter spec,json:integration-tests/report.json \
  --test-grep "$PATTERN" \
  "chrome:headless" \
  integration-tests/tests.js
