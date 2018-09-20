#!/usr/bin/env bash
. ./scripts/support/assert-in-container $0 $@

set -euo pipefail

test_url=http://localhost:8000/static/darkjs.bc.js
until $(curl --output /dev/null --silent --head --fail "${test_url}"); do
    printf '.'
    sleep 1
done

test_url=http://localhost:8000/static/elm.js
until $(curl --output /dev/null --silent --head --fail "${test_url}"); do
    printf '.'
    sleep 1
done

