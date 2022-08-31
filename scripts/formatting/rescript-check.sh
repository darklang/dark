#!/usr/bin/env bash

# Checks if a given ReScript file is formatted correctly

TEMPFILENAME=$(mktemp)
./node_modules/.bin/bsc -format $1 > "$TEMPFILENAME" && diff "$TEMPFILENAME" $1 && rm "$TEMPFILENAME"