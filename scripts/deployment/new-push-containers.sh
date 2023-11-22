#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

gcloud auth configure-docker us-central1-docker.pkg.dev
docker tag bwdserver:latest us-central1-docker.pkg.dev/darklang-next/production-containers/bwdserver:latest
docker image push us-central1-docker.pkg.dev/darklang-next/production-containers/bwdserver:latest
