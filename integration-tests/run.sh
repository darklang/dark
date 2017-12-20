#!/bin/bash

set -euo pipefail

set -x

phantomjs integration-tests/driveby.js "enter_changes_state"
