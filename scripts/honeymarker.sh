#!/usr/bin/env bash
set -euo pipefail

# Creates a 'marker' UI element in honeycomb; see
# https://docs.honeycomb.io/working-with-your-data/customizing-your-query/markers

# We didn't keep the honeycomb buildevent orb, but the api key is still in place
# for it
export HONEYCOMB_API_KEY=$BUILDEVENT_APIKEY

# Install honeymarker here
if [[ ! -f honeymarker ]]; then
    wget -q -O honeymarker https://honeycomb.io/download/honeymarker/linux/1.9 && \
      echo 'e74514a2baaf63a5828ff62ca2ca1aa86b3a4ab223ab6a7c53f969d7b55e37fb  honeymarker' | sha256sum -c && \
      chmod 755 ./honeymarker
fi

DATASETS="integration-tests
kubernetes-bwd-nginx
kubernetes-bwd-ocaml
kubernetes-cluster-events
kubernetes-resource-metrics
kubernetes-scheduler
kubernetes-stroller
postgres"

msg="${CIRCLE_USERNAME} deployed ${CIRCLE_PR_USERNAME}'s PR (${CIRCLE_BUILD_URL})"
url="${CIRCLE_PULL_REQUEST_URL}"

time for dataset in $DATASETS; do
    # Docs: https://docs.honeycomb.io/working-with-your-data/customizing-your-query/markers
    honeymarker --writekey="${HONEYCOMB_API_KEY}" \
        --dataset="$dataset" \
        --msg="${msg}" \
        --url="${url}" \
        --type="deploy" \
        --add
done
