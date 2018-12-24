#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "$@"

set -euo pipefail

PATTERN=".*"
SCRIPT=

for i in "$@"
do
  case "${i}" in
    --pattern=*)
    PATTERN=${1/--pattern=/''}
    ;;
    *)
    echo "Unexpected argument: $i"
    exit 255
    ;;
  esac
done

# Slowing this down massively slows down the test suite. If needed, we
# can change this on an individual action:
# https://devexpress.github.io/testcafe/documentation/test-api/actions/action-options.html#basic-action-options
SPEED=1

CONCURRENCY=4
if [[ -v CI ]]; then
  CONCURRENCY=1
fi

# Set up test reporters for CircleCI
TEST_RESULTS_DIR="${DARK_CONFIG_RUNDIR}/test_results"
TEST_RESULTS_JSON="${TEST_RESULTS_DIR}/integration_tests.json"
TEST_RESULTS_XML="${TEST_RESULTS_DIR}/integration_tests.xml"
mkdir -p "${TEST_RESULTS_DIR}"
REPORTERS=spec
REPORTERS+=,json:${TEST_RESULTS_JSON}
REPORTERS+=,xunit:${TEST_RESULTS_XML}

echo "Clearing old test files"
rm -f "${DARK_CONFIG_RUNDIR}/completed_tests/*"
rm -Rf "${DARK_CONFIG_RUNDIR}/screenshots/*"
rm -f "${TEST_RESULTS_DIR}/integration_tests.*"

# Clear DBs
DBLOG="${DARK_CONFIG_RUNDIR}/integration_db.log"
echo "Clearing old DB data (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"
function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" 2>&1; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

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

set +e # Dont fail immediately so that the sed is run

TEST_HOST="darklang.localhost:8000" \
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 200 \
    --speed "$SPEED" \
    --screenshots-on-fails \
    --screenshots "${DARK_CONFIG_RUNDIR}/screenshots/" \
    --concurrency "$CONCURRENCY" \
    --reporter "$REPORTERS" \
    --test-grep "$PATTERN" \
    "chrome:headless" \
    integration-tests/tests.js 2> "${DARK_CONFIG_RUNDIR}/integration_error.log"

RESULT=$?

# Fix xunit output for CircleCI flaky-tests stats
sed -i 's/ (screenshots: .*)"/"/' "${TEST_RESULTS_XML}"

exit $RESULT

