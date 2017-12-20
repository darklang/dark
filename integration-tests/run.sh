#!/bin/bash

set -euo pipefail

set -x

git checkout -- server/appdata/test_enter_changes_state.dark
git checkout -- server/appdata/test_field_access.dark

phantomjs integration-tests/driveby.js "enter_changes_state"
phantomjs integration-tests/driveby.js "field_access"
