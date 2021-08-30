#!/usr/bin/env bash
set -euo pipefail

# Creates a 'marker' UI element in honeycomb; see
# https://docs.honeycomb.io/working-with-your-data/customizing-your-query/markers

export HONEYCOMB_API_KEY=$BUILDEVENT_APIKEY

DATASETS="integration-tests
kubernetes-bwd-nginx
kubernetes-bwd-ocaml
kubernetes-cluster-events
kubernetes-garbagecollector
kubernetes-resource-metrics
kubernetes-scheduler
kubernetes-stroller
postgres"

msg="${CIRCLE_USERNAME} deployed PR (SHA1: ${CIRCLE_SHA1})"
url="${CIRCLE_BUILD_URL}"

time for dataset in $DATASETS; do
    # Docs: https://docs.honeycomb.io/working-with-your-data/customizing-your-query/markers
    honeymarker add \
        --writekey="${HONEYCOMB_API_KEY}" \
        --dataset="$dataset" \
        --msg="${msg}" \
        --url="${url}" \
        --type="deploy"
done
