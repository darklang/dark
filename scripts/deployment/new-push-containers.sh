#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

host="us-central1-docker.pkg.dev"
repo="$host/darklang-next/production-containers"

gcloud auth configure-docker "$repo"

docker tag bwdserver:latest "$repo/bwdserver:latest"
docker image push "$repo/bwdserver:latest"

# Digests are a property of the container in the registry, which is why we do
# this here
bwdserver_digest=$(docker inspect --format='{{index .RepoDigests 0}}' "$repo/bwdserver:latest")

cat << EOF > image-digests.json
[
  { "name": "bwdserver", "digest": "$bwdserver_digest" }
]
EOF
