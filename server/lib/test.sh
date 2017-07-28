#!/bin/bash

set -xeuo pipefail

# -------------
# Get the bearer token
# -------------
# CONSUMER_KEY="FOohmxHC3ExCFvLVbl8UySqFu"
# CONSUMER_SECRET="J5GgPfzqQvxSyjQFqP8MDE4diuZS87KtUDdhb1vBFA5BX0o7rP"
# concat="${CONSUMER_KEY}:${CONSUMER_SECRET}"

# curl 'https://api.twitter.com/oauth2/token' --data 'grant_type=client_credentials' -H 'application/x-www-form-urlencoded;charset=UTF-8' --user "$concat"

BEARER="AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuNyK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3Pagi8le92ZQE5PXTqimhtVRqyjeWRz"

# -------------
# Make API call
# -------------
curl -H "Authorization: Bearer ${BEARER}" -vv --get 'https://api.twitter.com/1.1/search/tweets.json' --data '&q=%23archaeology' --include
