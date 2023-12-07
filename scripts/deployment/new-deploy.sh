#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

# Read each entry from the JSON file and deploy the container
jq -c '.[]' image-digests.json | while IFS= read -r line; do
    container=$(echo "$line" | jq -r '.name')
    image=$(echo "$line" | jq -r '.digest')

    # ProdExec is only deployed on demand
    if [[ "$container" == "prodexec" ]]; then
        continue
    fi

    gcloud run services update "$container" \
      --project darklang-next \
      --region us-central1 \
      --image "$image"
done

