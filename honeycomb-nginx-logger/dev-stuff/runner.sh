#!/bin/bash

dirname="$(dirname "$(realpath "$0")")"

exec docker run \
    -e DATASETS_TO_LOG='sandbox|some-other-dataset' \
    -w /root \
    -v "${dirname}/installer.sh":/root/installer.sh \
    -v "${dirname}/sample-data.json":/root/sample-data.json \
    -v "${dirname}/honeytail-runner.sh":/root/honeytail-runner.sh \
    -it honeycomb-logger:latest
