#!/usr/bin/env bash

set -euo pipefail

YAML=services/honeycomb-agent/honeycomb-agent-config.yaml

set -x

docker run -v "$(pwd)/$YAML:/etc/honeycomb/config.yaml" honeycombio/honeycomb-kubernetes-agent:head --validate
