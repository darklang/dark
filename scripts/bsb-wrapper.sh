#!/usr/bin/env bash
# . ../scripts/support/assert-in-container "$0" "$@"

# note: no set -e
set -uo pipefail
set +e

error=0
errorline=

# run the last segment of a pipeline in the current shell. This allows getting
# the exit code of dune.
shopt -s lastpipe

unbuffer bsb "$@" 2>&1 | while read -r line; do
  # this error consistently breaks our compile, esp on CI
  if [[ "$line" == *"Fatal error: exception Unix.Unix_error(Unix.ENOENT, \"execv\", \"/home/dark/app/client/node_modules/bs-platform/lib/ninja.exe\")"* ]]; then
    error=1;
    errorline="$line";
  fi
  echo "$line";
done
result=$?

set -e
if [[ "$error" == 1 ]]; then
  echo "Ran into a weird bsb bug: $errorline"
  echo "Cleaning"
  cd .. && ./scripts/clear-bs-cache && cd client
  cd .. && ./scripts/clear-node-modules && cd client
  echo "Running again"
  ./scripts/retrying-yarn-install
  unbuffer bsb "$@"
else
  exit $result
fi
