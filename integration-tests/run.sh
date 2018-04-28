#!/usr/bin/env bash
set -euo pipefail
./scripts/support/assert-in-container $0 $@

set -euo pipefail

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
    *)
    echo "Unexpected argument: $i"
    exit -1
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
# When the container starts up, the first --full-restart takes 35s. This
# pkill fixes that.
sudo pkill dnsmasq
sudo service dnsmasq --full-restart

# Slowing this down massively slows down the test suite. If needed, we
# can change this on an individual action:
# https://devexpress.github.io/testcafe/documentation/test-api/actions/action-options.html#basic-action-options
# TODO: if this makes tests less flaky, remove the hack in tests.js
SPEED=0.4

echo "Clearing old test files"
rm -f ${DARK_CONFIG_RUN_DIR}/completed_tests/*
rm -Rf ${DARK_CONFIG_RUN_DIR}/screenshots/*
rm -f ${DARK_CONFIG_RUN_DIR}/integration.json
rm -Rf ${DARK_CONFIG_PERSIST_DIR}/events/test_*
rm -Rf ${DARK_CONFIG_PERSIST_DIR}/function_results/test_*

function exe { psql -d proddb -c "$@"; }

echo "Clearing test tables";
TESTDBS=$(psql -d proddb -q --command "SELECT table_name FROM information_schema.tables WHERE SUBSTRING(table_name, 0, 6) = 'test_';" | grep test_ || true)
SCRIPT="" # concated into on script for speed
for db in $TESTDBS; do
  SCRIPT+="DROP TABLE \"${db}\";";
done
exe "$SCRIPT"

echo "Clearing from migrations";
exe "DELETE FROM migrations WHERE SUBSTRING(host, 0, 6) = 'test_';"
echo "Clearing from oplists";
exe "DELETE FROM oplists WHERE SUBSTRING(host, 0, 6) = 'test_';"


TEST_HOST="integration-tests:$PORT" \
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 0 \
    --speed $SPEED \
    --screenshots-on-fails \
    --screenshots ${DARK_CONFIG_RUN_DIR}/screenshots/ \
    --concurrency 4 \
    --reporter spec,json:${DARK_CONFIG_RUN_DIR}/integration_tests.json \
    --test-grep "$PATTERN" \
    "chrome:headless" \
    integration-tests/tests.js
