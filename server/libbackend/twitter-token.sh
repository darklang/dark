#!/bin/bash

set -xeuo pipefail

# Note: we've deleted these keys.

# -------------
# Get the bearer token
# -------------
# CONSUMER_KEY=""
# CONSUMER_SECRET=""
# concat="${CONSUMER_KEY}:${CONSUMER_SECRET}"

# curl 'https://api.twitter.com/oauth2/token' --data 'grant_type=client_credentials' -H 'application/x-www-form-urlencoded;charset=UTF-8' --user "$concat"

BEARER=""

# -------------
# Make API call
# -------------
curl -H "Authorization: Bearer ${BEARER}" -vv --get 'https://api.twitter.com/1.1/search/tweets.json' --data '&q=%23archaeology' --include
