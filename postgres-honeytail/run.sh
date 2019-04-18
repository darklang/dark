#!/bin/bash
set -euo pipefail

# Auth: logs.py needs gcloud access (to the pubsub subscription).
# To get this locally, run docker with
# `-v $HOME/.config/gcloud:/root/.config/gcloud`
#
# honeytail needs an API key (unless DEBUG is set). This is the env var
# HONEYCOMB_WRITEKEY.
#
# Optional env vars: DATASET (defaults to bwd-postgres) and DEBUG

# For testing, you can run with DEBUG=1, and data will not be sent to honeycomb
# (--debug sets the log level, --debug_stdout says to write events to stdout
# instead of sending to honecomb)
if [[ "${DEBUG:-}" != "" ]]; then
    DEBUG_STDOUT=--debug_stdout
    DEBUG=--debug
fi

# In-cluster gcloud auth
if [[ "${GOOGLE_APPLICATION_CREDENTIALS_JSON:-}" != "" ]]; then
    echo $GOOGLE_APPLICATION_CREDENTIALS_JSON > service-account.key
    export GOOGLE_APPLICATION_CREDENTIALS=service-account.key
fi

python logs.py \
    | ./honeytail \
    ${DEBUG_STDOUT:-} \
    ${DEBUG:-} \
    -k="${HONEYCOMB_WRITEKEY:-unset}" \
    --dataset="${DATASET:-bwd-postgres}" \
    --parser=postgresql \
    --postgresql.log_line_prefix='[%t]: [%p]: [%l-1] db=%d,user=%u' \
    -f -
