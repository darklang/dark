#!/bin/bash

set -euo pipefail

set -x

git checkout -- server/appdata/test_enter_changes_state.dark
git checkout -- server/appdata/test_field_access.dark

testcafe --screenshots integration-tests/screenshots/ "chrome:headless" integration-tests/enter_changes_state.js
