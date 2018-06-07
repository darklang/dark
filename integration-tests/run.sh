#!/usr/bin/env bash
set -euo pipefail
. ./scripts/support/assert-in-container $0 $@

set -euo pipefail

PATTERN=".*"
GCP=
SCRIPT=

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
SPEED=0.8
if [[ -v CI ]]; then
  SPEED=0.4
fi
# Set up test reporters for CircleCI
TEST_RESULTS_DIR="${DARK_CONFIG_RUN_DIR}/test_results/"
mkdir -p "${TEST_RESULTS_DIR}"
REPORTERS=spec
REPORTERS+=,json:${TEST_RESULTS_DIR}/integration_tests.json
REPORTERS+=,xunit:${TEST_RESULTS_DIR}/integration_tests.xml

echo "Clearing old test files"
rm -f ${DARK_CONFIG_RUN_DIR}/completed_tests/*
rm -Rf ${DARK_CONFIG_RUN_DIR}/screenshots/*
rm -f ${TEST_RESULTS_DIR}/integration_tests.*
rm -Rf ${DARK_CONFIG_PERSIST_DIR}/events/test_*
rm -Rf ${DARK_CONFIG_PERSIST_DIR}/function_results/test_*
rm -Rf ${DARK_CONFIG_PERSIST_DIR}/function_results/test-*

# Clear DBs
function run_sql { psql -d proddb -c "$@" >> ${DARK_CONFIG_RUN_DIR}/integration_db.log 2>&1; }

function fetch_sql { psql -d proddb -t -c "$@"; }

echo "Clearing test schemas";
TESTSCHEMAS=$(fetch_sql "SELECT nspname FROM pg_catalog.pg_namespace WHERE SUBSTRING(nspname, 0, 16) = 'dark_user_test_';")
for db in $TESTSCHEMAS; do
  SCRIPT+="DROP SCHEMA \"${db}\" CASCADE;";
done

TESTSCHEMAS=$(fetch_sql "SELECT nspname FROM pg_catalog.pg_namespace WHERE SUBSTRING(nspname, 0, 16) = 'dark_user_test-';")
for db in $TESTSCHEMAS; do
  SCRIPT+="DROP SCHEMA \"${db}\" CASCADE;";
done

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE substring(name, 0, 6)
= 'test-';")
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM events where canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events where canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results where canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases where id = '$cid';";
done

run_sql "$SCRIPT";

echo "Clearing test data";
run_sql "DELETE FROM oplists WHERE SUBSTRING(host, 0, 6) = 'test_';";
run_sql "DELETE FROM oplists WHERE SUBSTRING(host, 0, 6) = 'test-';";
run_sql "DELETE FROM json_oplists WHERE SUBSTRING(host, 0, 6) = 'test-';";

TEST_HOST="integration-tests:$PORT" \
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 200 \
    --speed $SPEED \
    --screenshots-on-fails \
    --screenshots ${DARK_CONFIG_RUN_DIR}/screenshots/ \
    --concurrency 4 \
    --reporter $REPORTERS \
    --test-grep "$PATTERN" \
    "chrome:headless" \
    integration-tests/tests.js 2> ${DARK_CONFIG_RUN_DIR}/integration_error.log
