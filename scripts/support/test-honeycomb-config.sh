#!/bin/bash

set -eu
set pipefail

YAML=scripts/support/kubernetes/builtwithdark/honeycomb.yaml
IMAGE=$(yq -sr '.[4].spec.template.spec.containers[0].image' $YAML)

TMP_CONFIG=tmp_honeycomb_config.yaml
TMP_DOCKERFILE=tmp_honeycomb_config.Dockerfile
yq -sr '.[3].data."config.yaml"' $YAML > $TMP_CONFIG

# This is a little silly, but easier than figuring out how to put
# tmp_honeycomb_config.yaml in a volume for circle
echo "FROM $IMAGE" > $TMP_DOCKERFILE
echo "COPY $TMP_CONFIG /etc/honeycomb/config.yaml" >> $TMP_DOCKERFILE
docker build -t test-honeycomb -f $TMP_DOCKERFILE .

docker run -it test-honeycomb --validate

rm $TMP_CONFIG
rm $TMP_DOCKERFILE
