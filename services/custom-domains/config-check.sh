#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Check config is deployed correctly

set -euo pipefail

kubectl diff -f services/custom-domains/darkcustomdomain-ip-svc.yaml

# Note that services/custom-domains/darkcustomdomain-ingress.yaml is NOT checked!
#
# This ingress is `kubectl patch`'d to add new custom domains; `kubectl
# apply`ing the file over that would lose us those custom domains, so we don't
# want to do that.
