#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

# build all dockerfiles in containers/
for i in $(find containers -name Dockerfile); do
  echo "Building $i"
  # dir=$(dirname $i)
  # file=$(basename $i)
  docker build . -f $i
done