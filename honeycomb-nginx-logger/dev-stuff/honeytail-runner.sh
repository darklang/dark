#!/bin/bash

# --debug_stdout

honeytail \
  --api_host="http://localhost:8010" \
  --writekey=30d48ff3b877e92b1387d904a7823c67 \
  --dataset="sandbox" \
  --parser=json \
  --file "$(dirname "$0")/sample-data.json" \
  --backfill
