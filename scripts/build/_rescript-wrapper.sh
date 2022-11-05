#!/usr/bin/env bash
# . ../scripts/devcontainer/_assert-in-container "$0" "$@"

# note: no set -e
set -uo pipefail
set +e

error=0
errorline=

# run the last segment of a pipeline in the current shell. This allows getting
# the exit code of dune.
shopt -s lastpipe

NINJA_ANSI_FORCED=1 rescript "$@" 2>&1 | while read -r line; do
  # this error consistently breaks our compile, esp on CI
  if [[ "$line" == *"Fatal error: exception Unix.Unix_error(Unix.ENOENT, \"execv\", \"/home/dark/app/node_modules/bs-platform/lib/ninja.exe\")"* ]]; then
    error=1;
    errorline="$line";
  elif [[ "$line" == *"make inconsistent assumptions over interface"* ]]; then
    error=1;
    errorline="$line";
  fi
  echo "$line";
done
result=$?

set -e
if [[ "$error" == 1 ]]; then
  echo "Ran into a weird rescript bug: $errorline"
  echo "Cleaning"
  ./scripts/build/clear-rescript-cache
  ./scripts/build/clear-node-modules
  echo "Running again"
  ./scripts/npm-install-with-retry
  NINJA_ANSI_FORCED=1 rescript "$@"
else
  exit $result
fi
