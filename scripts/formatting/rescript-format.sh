#!/usr/bin/env bash

# Formats a given ReScript file in-place

TEMPFILENAME=$(mktemp)

./node_modules/.bin/bsc -format $1 > "$TEMPFILENAME" && mv "$TEMPFILENAME" $1