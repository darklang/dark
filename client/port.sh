#!/usr/bin/env bash

set -euo pipefail

cd client

for i in *.elm; do
  name=$(basename $i .elm).ml
  /Users/paulbiggar/.local/bin/elm-format $i --output ../client2/$name
done
