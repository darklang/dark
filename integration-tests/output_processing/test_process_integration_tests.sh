#!/bin/bash

# Not an automated test, this is a manual test to verify the script before
# pushing to circle
#
# If you pipe the output through `jq -s group_by(.error) | .[] | length'`, you
# should get "14\n1" - 14 non-error, 1 error (group_by sorts lexically, false is
# before true)
#
# (I have not put that in this script because this script is also useful for
# looking at what's in the output - the right keys, single- not multi- line
# json, etc.)

DIR=$(dirname "$0")

export CIRCLE_BRANCH="this is a test, lol"
export CIRCLE_BUILD_URL='this is a test of what happens with a "double quote"'

# SC2002 is "useless use of cat"
# shellcheck disable=SC2002
cat "${DIR}"/sample_integration_tests_with_failure.json\
    | scripts/support/process-integration-test-results.sh
