#!/usr/bin/env bash
set -euo pipefail

# This deliberately runs outside the container, in order to run Chrome on the
# host

PATTERN=".*"
DEBUG=false

BROWSER='unknown'
{
  PLATFORM=$(uname -s)
  if [[ $PLATFORM == "Darwin" ]]; then
    BROWSER="chrome"
  else
    BROWSER="chromium"
  fi
}

for i in "$@"
do
  case "${i}" in
    --pattern=*)
    PATTERN=${1/--pattern=/''}
    shift
    ;;
    --debug)
    DEBUG=true
    shift
    ;;
    *)
    echo "Unexpected argument: $i"
    exit 1
    ;;
  esac
done

# Prep (in the container)
./integration-tests/prep.sh

CONCURRENCY=1
if [[ -v IN_DEV_CONTAINER ]] || [[ "$DEBUG" == "true" ]]; then
  CONCURRENCY=1
fi

if [[ -v IN_DEV_CONTAINER ]]; then
  # Set up test reporters for CircleCI
  TEST_RESULTS_DIR="${DARK_CONFIG_RUNDIR}/test_results"
  TEST_RESULTS_JSON="${TEST_RESULTS_DIR}/integration_tests.json"
  TEST_RESULTS_XML="${TEST_RESULTS_DIR}/integration_tests.xml"
  REPORTERS=spec
  REPORTERS+=,json:${TEST_RESULTS_JSON}
  REPORTERS+=,xunit:${TEST_RESULTS_XML}
  XVFB_LOG="${DARK_CONFIG_RUNDIR}/logs/xvfb.log"

  export DISPLAY=:99.0
  # shellcheck disable=SC2024
  pgrep Xvfb > /dev/null || sudo Xvfb -ac :99 -screen 0 1600x1200x24 > "${XVFB_LOG}" 2>&1 &

  set +e # Dont fail immediately so that the sed is run

  unbuffer client/node_modules/.bin/testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 200 \
    --screenshots-on-fails \
    --screenshots "${DARK_CONFIG_RUNDIR}/screenshots/" \
    --concurrency "$CONCURRENCY" \
    --reporter "$REPORTERS" \
    --test-grep "$PATTERN" \
    "chrome \"--window-size=1600,1200\""  \
    integration-tests/tests.js 2>&1 | tee "${DARK_CONFIG_RUNDIR}/integration_error.log"

  RESULT=$?

  # Fix xunit output for CircleCI flaky-tests stats
  sed -i 's/ (screenshots: .*)"/"/' "${TEST_RESULTS_XML}"

  exit $RESULT
else
  if [[ "$DEBUG" == "true" ]]; then
    debugcmd="--debug-mode --inspect"
  else
    debugcmd=
  fi
  testcafe \
    --selector-timeout 50 \
    --assertion-timeout 50 \
    --app-init-delay 0 \
    --pageload-timeout 200 \
    --screenshots takeOnFails=true \
    --screenshots rundir/screenshots/ \
    --concurrency "$CONCURRENCY" \
    --reporter=spec \
    $debugcmd \
    --test-grep "$PATTERN" \
    $BROWSER \
    integration-tests/tests.js
fi
