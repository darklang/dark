#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# note: no set -e
set -uo pipefail
set +e

error=0
errorline=

# run the last segment of a pipeline in the current shell. This allows getting
# the exit code of dune.
shopt -s lastpipe

esy build dune "$@" 2>&1 | while read -r line; do
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
  rm -Rf _build/*
  echo "Running again"
  esy build dune "$@"
else
  exit $result
fi
