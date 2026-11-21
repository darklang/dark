#!/usr/bin/env bash
# The wasm-tools workload the browser build needs: emscripten, the browser runtime packs and
# the AOT compiler. Not in the dev image, which stays small for everyone who never builds this.
# About 1 GB, installed once per container.
set -euo pipefail
if ! dotnet workload list 2>/dev/null | grep -q "^wasm-tools "; then
  echo "installing the wasm-tools workload (once per container, a few minutes)"
  sudo dotnet workload install wasm-tools
fi
