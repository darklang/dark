#!/usr/bin/env bash
set -euo pipefail

# This deliberately runs outside the container, in order to run Chrome on the
# host

PATTERN=".*"
DEBUG=false

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

BROWSER='unknown'
{
  PLATFORM=$(uname -s)
  if [[ $PLATFORM == "Darwin" ]]; then
    if [[ "$DEBUG" == "true" ]]; then
      BROWSER='chrome:headless --window-size="1600,1200"'
    else
      BROWSER='chrome --window-size="1600,1200"'
    fi
  else
    BROWSER='chromium:headless --window-size="1600,1200"'
  fi
}

######################
# Prep (in the container)
######################
./integration-tests/prep.sh

######################
# Set up concurrency
######################
CONCURRENCY=3
if [[ "$DEBUG" == "true" ]]; then
  CONCURRENCY=1
elif [[ -v IN_DEV_CONTAINER ]]; then
  # This was caarefully measured in CI. 1x is much slower, 3x fails a lot.
  # Though perhaps with a larger machine 3x might work better.
  CONCURRENCY=2
fi

######################
# Run testcafe
######################
if [[ -v IN_DEV_CONTAINER ]]; then
  set +e # Dont fail immediately so that the sed is run

  echo "Starting testcafe"
  # shellcheck disable=SC2016
  unbuffer client/node_modules/.bin/testcafe \
    --concurrency "$CONCURRENCY" \
    --test-grep "$PATTERN" \
    --video rundir/videos \
    --video-options pathPattern='${TEST}-${QUARANTINE_ATTEMPT}.mp4' \
    "${BROWSER}" \
    integration-tests/tests.js 2>&1 | tee "${DARK_CONFIG_RUNDIR}/integration_error.log"

  RESULT=$?

  # Fix xunit output for CircleCI flaky-tests stats
  sed -i 's/ (screenshots: .*)"/"/' "rundir/test_results/integration_tests.xml"

  exit $RESULT
else

  # Check the version (matters when running outside the container)
  version=$(testcafe --version)
  expected_version=$(grep testcafe client/package.json | sed 's/\s*"testcafe": "//' | sed 's/",\s*//')
  if [[ "$version" != "$expected_version" ]]
  then
    echo "Incorrect version of testcafe: $version (expected $expected_version)"
    exit 1
  fi

  if [[ "$DEBUG" == "true" ]]; then
    debugcmd="--debug-mode --inspect"
  else
    debugcmd=
  fi
  # shellcheck disable=SC2016
  testcafe \
    --concurrency "$CONCURRENCY" \
    $debugcmd \
    --test-grep "$PATTERN" \
    --video rundir/videos \
    --video-options pathPattern='${TEST}-${QUARANTINE_ATTEMPT}.mp4' \
    "$BROWSER" \
    integration-tests/tests.js
fi
