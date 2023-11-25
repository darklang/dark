#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

set -x

gcloud run services update bwdserver \
  --project darklang-next \
  --region us-central1 \
  --image "us-central1-docker.pkg.dev/darklang-next/production-containers/bwdserver:$1"
