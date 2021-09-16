#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Check config is deployed correctly

set -euo pipefail

# We don't have the full file checked in for this, the rest comes from helm
kubectl create configmap honeycomb-agent \
  --from-file=config.yaml=services/honeycomb-agent/honeycomb-agent-config.yaml \
  --dry-run=client -o json \
  | kubectl diff -f -
