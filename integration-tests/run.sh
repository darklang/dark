#!/bin/bash

set -euo pipefail

# clean test files (reset repo files, delete others)
git checkout -- $(git ls-files server/appdata/test_*.dark)
rm $(git ls-files server/appdata/test_*.dark -o)


testcafe \
  --selector-timeout 50 \
  --assertion-timeout 50 \
  --app-init-delay 0 \
  --pageload-timeout 0 \
  --speed 1 \
  --screenshots-on-fails \
  --screenshots integration-tests/screenshots/ \
  "chrome:headless" \
  integration-tests/tests.js

# TODO: -c 5 (number of concurrent tests)
