#!/usr/bin/env bash

set -euo pipefail

cd client

# FILES=$(ls *.elm)
FILES=Main.elm

for i in "$FILES"; do
  newname=$(basename ${i,,} .elm).ml
  echo "$i -> $newname"
  /Users/paulbiggar/.local/bin/elm-format --elm-version 0.18 $i --output "../client2/src/${newname}"
done
