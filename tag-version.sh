#!/bin/sh

set -euo pipefail

git rev-parse --short HEAD | \
    # Remove trailing endline
    tr -d '\n'
