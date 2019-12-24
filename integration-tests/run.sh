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
    BROWSER='chrome --window-size="1600,1200"'
  else
    BROWSER='chromium --window-size="1600,1200"'
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

######################
# Prep (in the container)
######################
./integration-tests/prep.sh

######################
# Set up concurrency
######################
CONCURRENCY=1
if [[ -v IN_DEV_CONTAINER ]] || [[ "$DEBUG" == "true" ]]; then
  CONCURRENCY=1
fi

######################
# Run testcafe
######################
if [[ -v IN_DEV_CONTAINER ]]; then
  # Set up test reporters for CircleCI
  XVFB_LOG="${DARK_CONFIG_RUNDIR}/logs/xvfb.log"

  export DISPLAY=:99.0
  # shellcheck disable=SC2024
  echo "Starting Xvfb"
  pgrep Xvfb > /dev/null || sudo Xvfb -ac :99 -screen 0 1600x1200x24 | sudo tee "${XVFB_LOG}" 2>&1 &
  echo "Waiting for Xvfb to be ready..."
  while ! xdpyinfo -display ${DISPLAY} > /dev/null ; do
    echo -n ''
    sleep 1
  done

  set +e # Dont fail immediately so that the sed is run

  echo "Starting testcafe"
  unbuffer client/node_modules/.bin/testcafe \
    --concurrency "$CONCURRENCY" \
    --test-grep "$PATTERN" \
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
  testcafe \
    --concurrency "$CONCURRENCY" \
    $debugcmd \
    --test-grep "$PATTERN" \
    --video rundir/videos \
    "$BROWSER" \
    integration-tests/tests.js
fi
