#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

# Output looks like:
# ```
# {"name":"execute_function_works","durationMs":41156,"unstable":false,"skipped":false,"error":true}
# {"name":"int_add_with_float_error_includes_fnname","durationMs":9341,"skipped":false,"error":false}
# ```,
# plus some data from circle's environment variables - see the `grep`
# line for the complete list. You can repro the above json with
# integration-tests/output_processing/test_process_integration_tests.sh, which
# also adds dummy values for CIRCLE_BRANCH and CIRCLE_BUILD_URL

# Uses multiple invocations of jq so we can provide a comment for each line.
#
# 1. Get tests
# 2. Add a error:bool field so it's easy to filter in honeycomb
# 3. Remove fields that aren't useful to us.
#    (In my sample input, meta has always been empty, and .errs is an array with
#    a string of error output. It's long.)
# 4. Get env vars (set by circle) that we care about and add them to the json
#    Documentation of these vars in circle can be found at
#    https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables
#    Note that we sanitize _very_ aggressively to avoid having to bother
#    with string-escaping
# 5. honeytail requires single-line json objects,
#    multiline json will break it,
# pending https://github.com/honeycombio/honeytail/pull/133
jq '.suites[0].suites[0][]' \
    | jq '. | [.[] | del(.tests) + .tests[0]]' \
    | jq '. | [.[] | del(.results) + .results[0]]' \
    | jq '. | [.[] | del(.tags, .annotations, .attachments, .stdout, .stderr)]' \
    | jq --argfile env_vars <(env \
            | grep -e CIRCLE_BRANCH \
                   -e CIRCLE_BUILD_URL \
                   -e CIRCLE_NODE_INDEX \
                   -e CIRCLE_PR_NUMBER \
                   -e CIRCLE_PR_USERNAME \
                   -e CIRCLE_PULL_REQUEST \
                   -e CIRCLE_SHA1 \
                   -e CIRCLE_USERNAME \
            | sed -e 's/[^A-Za-z0-9_ =:/.-]//g' \
            | sed -e 's/\(.*\)=\(.*\)/{"\1": "\2"}/' \
            | jq -s add) \
        '. + $env_vars' \
    | jq -c '.'
