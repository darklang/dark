#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

registry="us-central1-docker.pkg.dev"
gcloud auth configure-docker "$registry"

repo="$registry/darklang-next/production-containers"

docker tag bwdserver:latest "$repo/bwdserver:latest"
docker image push "$repo/bwdserver:latest"

docker tag prodexec:latest "$repo/prodexec:latest"
docker image push "$repo/prodexec:latest"

# Digests are a property of the container in the registry, which is why we do
# this here
bwdserver_digest=$(docker inspect --format='{{index .RepoDigests 0}}' "$repo/bwdserver:latest")
prodexec_digest=$(docker inspect --format='{{index .RepoDigests 0}}' "$repo/prodexec:latest")

cat << EOF > image-digests.json
[
  { "name": "bwdserver", "digest": "$bwdserver_digest" },
  { "name": "prodexec", "digest": "$prodexec_digest" }
]
EOF
