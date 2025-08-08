#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# builds the `darklang` exes, for all supported platforms

# TODO run some test in CI against the current-runtime executable,
# just verifying that it (incl the packaged parser) works.


set -euo pipefail

sha=$(git rev-parse HEAD | cut -c 1-10)

release="alpha-$sha"

# Export GIT_COMMIT so the built binary includes the git hash
export GIT_COMMIT="$sha"

mkdir -p clis
rm -rf clis/.darklang

# Force WAL checkpoint to ensure all data is committed to main DB before embedding
echo "Forcing WAL checkpoint to commit all data before embedding database..."
sqlite3 rundir/data.db "PRAGMA wal_checkpoint(TRUNCATE);" || echo "WAL checkpoint completed (or no WAL file exists)"

if [[ " $* " =~ " --cross-compile " ]]; then
  echo "Cross-compiling for all supported runtimes"

  # If cross-compiling, include all supported architectures.
  # This list must match the list of runtime identifiers in
  # - `backend/src/LibTreeSitter/LibTreeSitter.fsproj`
  # - and `./scripts/build/build-tree-sitter.sh`.
  #
  # Otherwise we'll fail to include the correct native library
  # for the relevant runtime.
  runtimes="linux-x64 linux-musl-x64 linux-arm64 linux-arm osx-x64 osx-arm64 win-x64 win-arm64"
  #runtimes="linux-x64" # sometimes this is set, locally, for testing

  # Parallelism set to 1 here to avoid running out of memory.
  # TODO: do better with gnu parallel or with this solution that I couldn't make work:
  # https://stackoverflow.com/a/43951971/104021
  for rt in $runtimes; do
    echo "Building for runtime: $rt"
    ./scripts/build/_dotnet-wrapper publish \
      -c Release \
      src/Cli/Cli.fsproj \
      /p:DebugType=None \
      /p:DebugSymbols=false \
      /p:PublishSingleFile=true \
      /p:PublishTrimmed=true \
      /p:PublishReadyToRun=true \
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

else
  # Building for the default runtime based on the current environment
  echo "Building for the default runtime"

  runtime=""
  machine_arch=$(uname -m)

  case "$machine_arch" in
    x86_64)
      runtime="linux-x64"
      ;;
    aarch64)
      runtime="linux-arm64"
      ;;
    *)
      echo "Unsupported machine architecture: $machine_arch"
      exit 1
      ;;
  esac

  ./scripts/build/_dotnet-wrapper publish \
    -c Release \
    src/Cli/Cli.fsproj \
    /p:DebugType=None \
    /p:DebugSymbols=false \
    /p:PublishSingleFile=true \
    /p:PublishTrimmed=true \
    /p:PublishReadyToRun=true \
    --self-contained true

  target="clis/darklang-$release-$runtime"
  echo "Moving to $target"
  mv -f "backend/Build/out/Cli/Release/net8.0/$runtime/publish/Cli" "$target"

  # we probably want to execute this right away if we're not cross-compiling,
  # so let's not bother gzipping
fi