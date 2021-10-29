#!/usr/bin/env bash
set -euo pipefail

# This deliberately can be run outside the container - sometimes you want to test on
# the host

PATTERN=".*"
DEBUG_MODE_FLAG=""
CONCURRENCY=1
RETRIES=0
BASE_URL="http://darklang.localhost:8000"

for i in "$@"
do
  case "${i}" in
    --pattern=*)
    PATTERN=${1/--pattern=/''}
    shift
    ;;
    --retry)
    RETRIES=2
    shift
    ;;
    --concurrency=*)
    CONCURRENCY=${1/--concurrency=/''}
    shift
    ;;
    --debug)
    DEBUG_MODE_FLAG="--debug"
    shift
    ;;
    --fsharp)
    BASE_URL="http://darklang.localhost:9000"
    shift
    ;;
    *)
    echo "Unexpected argument: $i"
    exit 1
    ;;
  esac
done

BROWSER="chromium"

######################
# Prep (in the container)
######################
./integration-tests/prep.sh

######################
# Set up concurrency
######################
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
# Run playwright
######################

if [[ ! -d "integration-tests/node_modules" ]]; then
  echo "Packages not installed: run \`npm install\` in integration-tests"
  exit 1
fi

# Check the installed version (matters when running outside the container)
extract_version() { grep -Eo '[0-9].[-.0-9rc]+'; }
expected_version=$(grep playwright/test integration-tests/package.json | extract_version)
version=$(integration-tests/node_modules/.bin/playwright --version | extract_version)

if [[ "$version" != "$expected_version" ]]
then
  echo "Incorrect version of playwright: '$version' (expected '$expected_version')"
  exit 1
fi

if [[ -v IN_DEV_CONTAINER ]]; then
  echo "Starting playwright"
  integration-tests/node_modules/.bin/playwright --version
  integration-tests/node_modules/.bin/playwright \
    test \
    --workers "$CONCURRENCY" \
    --grep "$PATTERN" \
    --browser "${BROWSER}" \
    --output "${DARK_CONFIG_RUNDIR}/integration-tests/" \
    --retries "$RETRIES" \
    --config integration-tests/playwright.config.ts

  if [[ -v CI ]]; then
    integration-tests/_integration-test-results-to-honeycomb-playwright.sh
  fi
  exit $RESULT
else
  integration-tests/node_modules/.bin/playwright --version
  BASE_URL="$BASE_URL" integration-tests/node_modules/.bin/playwright test \
    $DEBUG_MODE_FLAG \
    --browser "${BROWSER}" \
    --output "rundir/integration-tests/" \
    --workers "$CONCURRENCY" \
    --grep "$PATTERN" \
    --retries "$RETRIES" \
    --config integration-tests/playwright.config.ts

fi
