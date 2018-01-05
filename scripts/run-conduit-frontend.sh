#!/bin/bash

set -euo pipefail

set -x

if [ ! -d ../conduit-frontend ]; then
  # cd ../conduit-frontend && git pull
  pushd .. && git clone git@github.com:darklang/conduit-frontend && popd
fi

./scripts/run-in-docker scripts/run-conduit-frontend-in-container.sh
