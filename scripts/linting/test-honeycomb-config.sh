#!/usr/bin/env bash

# Validate the honeycomb agent config

# Creates a docker container with the agent and the config in it and runs it to
# validate the config. The honeycomb docs suggest using docker volumes, but that
# doesn't work with remote docker hosts, like we use in CI.

set -euo pipefail

YAML=services/honeycomb-agent/config.yaml

IMAGE=honeycombio/honeycomb-kubernetes-agent:head

TMP_DOCKERFILE=tmp_honeycomb_config.Dockerfile

# This is a little silly, but easier than figuring out how to put
# tmp_honeycomb_config.yaml in a volume for circle
echo "FROM $IMAGE" > $TMP_DOCKERFILE
echo "COPY $YAML /etc/honeycomb/config.yaml" >> $TMP_DOCKERFILE
docker build -t test-honeycomb -f $TMP_DOCKERFILE .

docker run -it test-honeycomb --validate

rm $TMP_DOCKERFILE
