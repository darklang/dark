#!/usr/bin/env bash
set -euo pipefail
./scripts/support/assert-in-container $0

PATTERN=".*"
GCP=

for i in "$@"
do
  case "${i}" in
    --gcp)
    GCP=y; shift
    ;;
    --pattern=*)
    PATTERN=${1/--pattern=/''}
    ;;
  esac
done

if [[ $GCP == "y" ]]; then
  echo "Running integration tests on GCP container"
  DOMAIN="dark-local-gcp"
  PORT="80"
  IP=$(dig @localhost $DOMAIN | grep "^$DOMAIN" | cut -f6)
else
  echo "Running integration tests on dev server"
  IP="127.0.0.1"
  PORT="8000"
fi

# Set up wildcard for this domain via dnsmasq for this host. You would
# think we'd just do a CNAME, but a CNAME only work on qualified
# domains, and Docker doesn't make qualified domains.
echo "Setting up DNS for integration-tests -> $IP"
CONF="address=/integration-tests/$IP"
echo "$CONF" | sudo tee /etc/dnsmasq.d/dnsmasq-integration-tests.conf
sudo service dnsmasq --full-restart

# Slowing this down massively slows down the test suite. If needed, we
# can change this on an individual action:
# https://devexpress.github.io/testcafe/documentation/test-api/actions/action-options.html#basic-action-options
SPEED=1

rm -Rf "${DARK_CONFIG_PERSIST_DIR}/completed_tests"

TEST_HOST="integration-tests:$PORT" \
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 0 \
    --speed $SPEED \
    --screenshots-on-fails \
    --screenshots $DARK_CONFIG_RUN_DIR/screenshots/ \
    --concurrency 4 \
    --reporter spec,json:$DARK_CONFIG_RUN_DIR/integration_tests.json \
    --test-grep "$PATTERN" \
    "chrome:headless" \
    integration-tests/tests.js
