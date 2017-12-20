#!/bin/bash

set -euo pipefail

set -x

phantomjs integration-tests/driveby.js "enterChangeState"
