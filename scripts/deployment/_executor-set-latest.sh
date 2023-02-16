#!/usr/bin/env bash

set -euo pipefail

if [[ -v DARKLANG_EXECUTOR_SECRET ]]; then
  secret=$DARKLANG_EXECUTOR_SECRET
else
  echo "DARKLANG_EXECUTOR_SECRET not set"
  exit 1
fi

hash=$(git rev-parse --short HEAD)

curl -X POST -H 'Content-Type: application/json' https://editor.darklang.com/latest-executor -H "Authorization:Bearer $secret" -d "{ \"hash\": \"$hash\" }"

echo
