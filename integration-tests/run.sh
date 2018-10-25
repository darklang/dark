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
echo "address=/integration-tests/$IP" | sudo tee /etc/dnsmasq.d/dnsmasq-integration-tests.conf
echo "address=/$DARK_CONFIG_STATIC_HOST/$IP" | sudo tee -a /etc/dnsmasq.d/dnsmasq-integration-tests.conf
echo "address=/$DARK_CONFIG_USER_CONTENT_HOST/$IP" | sudo tee -a /etc/dnsmasq.d/dnsmasq-integration-tests.conf
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

CONCURRENCY=4
if [[ -v CI ]]; then
  CONCURRENCY=1
fi

# Set up test reporters for CircleCI
TEST_RESULTS_DIR="${DARK_CONFIG_RUN_DIR}/test_results"
TEST_RESULTS_JSON="${TEST_RESULTS_DIR}/integration_tests.json"
TEST_RESULTS_XML="${TEST_RESULTS_DIR}/integration_tests.xml"
mkdir -p "${TEST_RESULTS_DIR}"
REPORTERS=spec
REPORTERS+=,json:${TEST_RESULTS_JSON}
REPORTERS+=,xunit:${TEST_RESULTS_XML}

echo "Clearing old test files"
rm -f ${DARK_CONFIG_RUN_DIR}/completed_tests/*
rm -Rf ${DARK_CONFIG_RUN_DIR}/screenshots/*
rm -f ${TEST_RESULTS_DIR}/integration_tests.*

# Clear DBs
DBLOG="${DARK_CONFIG_RUN_DIR}/integration_db.log"
echo "Clearing old DB data (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"
function run_sql { psql -d $DB -c "$@" >> "$DBLOG" 2>&1; }

function fetch_sql { psql -d $DB -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE substring(name, 0, 6)
= 'test-';")
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM events WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases WHERE id = '$cid';";
done
run_sql "$SCRIPT";

# TODO(bucklescript): remove after ship
# Set branch for testcafe to look at
export BRANCH=$(git rev-parse --abbrev-ref HEAD)

set +e # Dont fail immediately so that the sed is run

TEST_HOST="integration-tests:$PORT" \
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 200 \
    --speed $SPEED \
    --screenshots-on-fails \
    --screenshots ${DARK_CONFIG_RUN_DIR}/screenshots/ \
    --concurrency "$CONCURRENCY" \
    --reporter $REPORTERS \
    --test-grep "$PATTERN" \
    "chrome:headless" \
    integration-tests/tests.js 2> ${DARK_CONFIG_RUN_DIR}/integration_error.log

RESULT=$?

# Fix xunit output for CircleCI flaky-tests stats
sed -i 's/ (screenshots: .*)"/"/' ${TEST_RESULTS_XML}

# TODO(bucklescript): remove after ship
# Intent: make integration tests report legitimate result when running elm tests
# and return 0 for bucklescript
if [[ -v $CI && "$BRANCH" == "master" ]]; then
  exit $RESULT
else
  exit 0
fi

