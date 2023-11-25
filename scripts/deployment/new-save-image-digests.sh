#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

# Container/service names
containers=("bwdserver")

output_file="image-digests.json"

# Start JSON
echo "[" > "$output_file"

for container in "${containers[@]}"; do
    digest=$(docker inspect --format='{{index .RepoDigests 0}}' "$container":latest)
    echo "{\"name\": \"$container\", \"digest\": \"$digest\"}," >> "$output_file"
done

# Remove the last comma and close the JSON array
sed -i '$ s/,$//' "$output_file"
echo "]" >> "$output_file"

# Notify user
echo "Digests saved to $output_file"
