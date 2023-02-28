#!/usr/bin/env bash

# build all dotnet executor binaries in parallel

set -euo pipefail

# Parallelism set to 1 here to avoid running out of memory.
# TODO: do better with gnu parallel or with this solution that I couldn't make work:
# https://stackoverflow.com/a/43951971/104021
runtimes="linux-x64 linux-musl-x64 linux-arm64 osx-x64 osx-arm64 win-x64 win-arm64"
echo $runtimes \
  | xargs \
      --verbose \
      --max-args 1 \
      --max-procs 1 \
      ./scripts/build/_dotnet-wrapper publish \
          -c Release \
          src/Executor/Executor.fsproj \
          /p:DebugType=None \
          /p:DebugSymbols=false \
          /p:PublishSingleFile=true \
          /p:PublishTrimmed=true \
          /p:PublishReadyToRun=false \
          --self-contained true \
          --runtime