#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

# Read each entry from the JSON file and deploy the container
jq -c '.[]' image-digests.json | while IFS= read -r line; do
    container=$(echo "$line" | jq -r '.name')
    digest=$(echo "$line" | jq -r '.digest')

    gcloud run services update $container \
      --project darklang-next \
      --region us-central1 \
      --image "us-central1-docker.pkg.dev/darklang-next/production-containers/$container:$digest"
done

