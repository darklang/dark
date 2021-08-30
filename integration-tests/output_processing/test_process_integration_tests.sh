#!/bin/bash

# Not an automated test, this is a manual test to verify the script before
# pushing to circle
#
# If you run this script with SUMMARY=1, you should get "1\n1", indicating one
# error case and one success.
#
# (That is not the default because this script is also useful for looking at
# what's in the output - the right keys, single- not multi- line json, etc.)

DIR=$(dirname "$0")

export CIRCLE_BRANCH="this is a test, lol"
export CIRCLE_BUILD_URL='this is a test of what happens with a "double quote"'

if [[ -z $SUMMARY ]]; then
    SUMMARY_CMD="cat"
else
    SUMMARY_CMD=(jq -s 'group_by(.error) | .[] | length')
fi

# SC2002 is "useless use of cat"
# shellcheck disable=SC2002
cat "${DIR}"/sample_integration_tests_with_failure.json\
    | scripts/deployment/process-integration-test-results.sh \
    | "${SUMMARY_CMD[@]}"
