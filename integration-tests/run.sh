#!/usr/bin/env bash

# Run playwright integration tests

set -euo pipefail

# This deliberately can be run outside the container - sometimes you want to test on
# the host. TODO this is currently failing, unless you specify some env vars
# explicitly. Here's a sample command to get it working for now:
# DARK_CONFIG_APISERVER_HOST=darklang.localhost:9000 DARK_CONFIG_BWDSERVER_HOST=builtwithdark.localhost:11001 ./integration-tests/run.sh

# CLEANUP this script fails if you cd to this directory.
# we should adjust to either allow such, or warn so dev doesn't get confused

PATTERN=".*"
DEBUG_MODE_FLAG=""
CONCURRENCY=1
RETRIES=0
REPEAT=1 # repeat allows us to repeat individual tests many times to check for edge cases
BASE_URL="http://${DARK_CONFIG_APISERVER_HOST}"
BWD_BASE_URL=".${DARK_CONFIG_BWDSERVER_HOST}"
BROWSER="chromium"
PUBLISHED=""

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
    --repeat=*)
    REPEAT=${1/--repeat=/''}
    shift
    ;;
    --published)
    PUBLISHED="--published"
    shift
    ;;
    --debug)
    DEBUG_MODE_FLAG="--debug"
    shift
    ;;
    --ocaml)
    BASE_URL="http://darklang.localhost:8000"
    BWD_BASE_URL=".builtwithdark.lvh.me:8000"
    shift
    ;;
    *)
    echo "Unexpected argument: $i"
    exit 1
    ;;
  esac
done


######################
# Check the version
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

######################
# Prep for tests (in the container)
######################
./integration-tests/prep.sh

# We need to restart the server after adding new packages. Integration tests test
# against the dev environment, not the test one.
./scripts/run-fsharp-server "${PUBLISHED}" --restart=no
./scripts/devcontainer/_wait-until-apiserver-ready

######################
# Run playwright
######################
echo "Starting playwright"
integration-tests/node_modules/.bin/playwright --version

set -x
set +e # We're going to use this error code

BASE_URL="$BASE_URL" BWD_BASE_URL="$BWD_BASE_URL" DEBUG=pw:test integration-tests/node_modules/.bin/playwright \
  test \
  $DEBUG_MODE_FLAG \
  --workers "$CONCURRENCY" \
  --grep "$PATTERN" \
  --browser "${BROWSER}" \
  --repeat-each "${REPEAT}" \
  --output "rundir/integration-tests/" \
  --retries "$RETRIES" \
  --config integration-tests/playwright.config.ts

STATUS=$?

if [[ $STATUS -ne 0 ]]; then
  echo "Playwright tests failed"
  echo "The following tests were skipped and shouldn't have been (if the integration test timed out, these are likely culprits):"
  cat rundir/test_results/integration_tests.json | jq -r '.suites[0].suites[0].specs[] | select( .tests[0].results[0].status == "skipped") | .title ' | sort

  exit $STATUS
fi

