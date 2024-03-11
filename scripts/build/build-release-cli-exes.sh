#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# build all dotnet executor binaries in parallel

set -euo pipefail

sha=$(git rev-parse HEAD)

release="alpha-$sha"

mkdir -p clis

# Parallelism set to 1 here to avoid running out of memory.
# TODO: do better with gnu parallel or with this solution that I couldn't make work:
# https://stackoverflow.com/a/43951971/104021

# This list must match the list of runtime identifiers in
# `build-release-cli-exes.sh`! Otherwise we'll fail to include
# the correct native library for the relevant runtime.
runtimes="linux-x64 linux-musl-x64 linux-arm64 linux-arm osx-x64 osx-arm64"
# TODO: include `win-x64` and `win-arm64`.

for rt in $runtimes; do
  echo "Building for runtime: $rt"

  ./scripts/build/_dotnet-wrapper publish \
    -c Release \
    src/Cli/Cli.fsproj \
    /p:DebugType=None \
    /p:DebugSymbols=false \
    /p:PublishSingleFile=true \
    /p:PublishTrimmed=true \
    /p:PublishReadyToRun=false \
    --self-contained true \
    --runtime "$rt"

  target="clis/darklang-$release-$rt"
  echo "Moving to $target"
  if [[ $rt == win-* ]]; then
    mv -f "backend/Build/out/Cli/Release/net8.0/$rt/publish/Cli.exe" "$target.exe"
    gzip -f "$target.exe"
  else
    mv -f "backend/Build/out/Cli/Release/net8.0/$rt/publish/Cli" "$target"
    gzip -f "$target"
  fi
done
