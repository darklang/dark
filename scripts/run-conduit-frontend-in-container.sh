#!/bin/bash

set -euo pipefail

cd ~/conduit-frontend
elm-live --host=0.0.0.0 --output=elm.js src/Main.elm --pushstate --open --debug
