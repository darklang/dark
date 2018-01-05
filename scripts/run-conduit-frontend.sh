#!/bin/bash

set -euo pipefail

set -x

./scripts/run-in-docker scripts/run-conduit-frontend-in-container.sh
