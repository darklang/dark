#!/bin/bash

set -euo pipefail

set -x

pushd client/integration-tests  \
  && elm-make --yes --output ../../integration-tests/tests.js \
  && popd

phantomjs integration-tests/driveby.js
