#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "$@"

# note: no set -e
set -uo pipefail
set +e

error=0
errorline=

# run the last segment of a pipeline in the current shell. This allows getting
# the exit code of dune.
shopt -s lastpipe

unbuffer esy build --project /home/dark/app/backend dune "$@" 2>&1 | while read -r line; do
  # this error consistently breaks our compile, esp on CI
  if [[ "$line" == *"make inconsistent assumptions over "* ]]; then
    error=1;
    errorline="$line";
  fi
  echo "$line";
done
result=$?

set -e
if [[ "$error" == 1 ]]; then
  echo "Running dune caused an error: $errorline"
  echo "Cleaning"
  rm -Rf backend/_build/*
  echo "Running again"
  unbuffer esy build --project /home/dark/app/backend dune "$@"
else
  exit $result
fi
