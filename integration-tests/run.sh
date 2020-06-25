#!/usr/bin/env bash
set -euo pipefail

# This deliberately runs outside the container, in order to run Chrome on the
# host

PATTERN=".*"
DEBUG=false
DEBUG_MODE_FLAG=""

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
    --debug-mode)
    DEBUG_MODE_FLAG="--debug-mode"
    shift
    ;;
    *)
    echo "Unexpected argument: $i"
    exit 1
    ;;
  esac
done

BROWSER='unknown'
{
  if [[ "$DEBUG" == "true" || "$DEBUG_MODE_FLAG" == "--debug-mode" ]]; then
    BROWSER='chrome --window-size="1600,1200"'
  else
    BROWSER='chrome:headless --window-size="1600,1200"'
  fi
}

######################
# Prep (in the container)
######################
./integration-tests/prep.sh

######################
# Set up concurrency
######################
CONCURRENCY=1
# Temporarily disabled until we sort out concurrency-related problems
# @dstrelau 2020-02-18
# if [[ "$DEBUG" == "true" ]]; then
#   CONCURRENCY=1
# elif [[ -v IN_DEV_CONTAINER ]]; then
#   # This was caarefully measured in CI. 1x is much slower, 3x fails a lot.
#   # Though perhaps with a larger machine 3x might work better.
#   CONCURRENCY=2
# fi


######################
# Run testcafe
######################

# Check the testcafe version (matters when running outside the container)
extract_version() { grep -Eo '[0-9].[-.0-9rc]+'; }
expected_version=$(grep testcafe package.json | extract_version)

if [[ -v IN_DEV_CONTAINER ]]; then
  set +e # Dont fail immediately so that the sed is run

  echo "Starting testcafe"
  npx "testcafe@${expected_version}" --version
  # shellcheck disable=SC2016
  unbuffer npx "testcafe@${expected_version}" \
    --concurrency "$CONCURRENCY" \
    --test-grep "$PATTERN" \
    --video rundir/videos \
    --video-options pathPattern='${TEST}-${QUARANTINE_ATTEMPT}.mp4' \
    "${BROWSER}" \
    integration-tests/tests.js 2>&1 | tee "${DARK_CONFIG_RUNDIR}/integration_error.log"

  RESULT=$?

  # Fix xunit output for CircleCI flaky-tests stats
  sed -i 's/ (screenshots: .*)"/"/' "rundir/test_results/integration_tests.xml"
  
  if [[ -v CI ]]; then
    scripts/support/integration-test-results-to-honeycomb.sh
  fi
  exit $RESULT
else
  version=$(testcafe --version | extract_version)
  if [[ "$version" != "$expected_version" ]]
  then
    echo "Incorrect version of testcafe: '$version' (expected '$expected_version')"
    exit 1
  fi

  # Check the node version (matters when running outside the container)
  version=$(node -v | sed 's/^v\([0-9]*\)\..*/\1/')
  if [[ "$version" -lt 10 ]]
  then
    # With node v8, I get "ReferenceError: URL is not defined";
    # We don't use lighthouse, but
    # https://github.com/GoogleChrome/lighthouse/issues/8909 suggests this means
    # we need node >= 10
    echo "Incorrect version of node: '$(node -v)' (expected node >= 10)"
    exit 1
  fi

  # shellcheck disable=SC2016
  testcafe \
    $DEBUG_MODE_FLAG \
    --concurrency "$CONCURRENCY" \
    --test-grep "$PATTERN" \
    --video rundir/videos \
    --video-options pathPattern='${TEST}-${QUARANTINE_ATTEMPT}.mp4' \
    "$BROWSER" \
    integration-tests/tests.js
fi
