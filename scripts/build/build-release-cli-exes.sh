#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Builds the `darklang` CLI exe(s) for specified or default runtime(s).
#
# Usage:
#   ./scripts/build/build-release-cli-exes.sh                    # Build for current container runtime
#   ./scripts/build/build-release-cli-exes.sh --runtimes=all     # Build for all supported runtimes
#   ./scripts/build/build-release-cli-exes.sh --runtimes=linux-x64,osx-arm64  # Build for specific runtimes
#   ./scripts/build/build-release-cli-exes.sh --aot              # AOT-publish (native, no JIT)
#
# Supported runtimes: linux-x64, linux-musl-x64, linux-arm64, linux-arm, osx-x64, osx-arm64, win-x64, win-arm64

set -euo pipefail

# Parse arguments
RUNTIMES_ARG=""
GZIP_OUTPUT=false
AOT=false

for arg in "$@"; do
  case $arg in
    --runtimes=*)
      RUNTIMES_ARG="${arg#*=}"
      ;;
    --cross-compile)
      # Legacy flag - treat as --runtimes=all
      RUNTIMES_ARG="all"
      ;;
    --gzip)
      GZIP_OUTPUT=true
      ;;
    --aot)
      # AOT publish: native code, no JIT, smaller + faster cold start.
      # Mutually exclusive with PublishReadyToRun (which we drop below).
      AOT=true
      ;;
    *)
      echo "Unknown argument: $arg"
      exit 1
      ;;
  esac
done

sha=$(git rev-parse HEAD | cut -c 1-10)
release="alpha-$sha"

# Export GIT_COMMIT so the built binary includes the git hash
export GIT_COMMIT="$sha"

mkdir -p clis
rm -rf clis/.darklang

# Export a seed (smaller DB) and use it as the embedded data.db for smaller exes.
# The seed has full schema but no derived data — the grow step rebuilds on first run.
# We intentionally ship the slim seed (not a pre-projected DB) so every user
# has the same boot path and the ops are present for rewind/inspection.
echo "Exporting seed for embedding..."
sqlite3 rundir/data.db "PRAGMA wal_checkpoint(TRUNCATE);" || true
scripts/run-local-exec export-seed rundir/seed.db
cp rundir/seed.db rundir/data.db
echo "Replaced data.db with seed ($(du -h rundir/data.db | cut -f1))"

# All supported runtimes - must match:
# - backend/src/LibTreeSitter/LibTreeSitter.fsproj
# - ./scripts/build/build-tree-sitter.sh
ALL_RUNTIMES="linux-x64 linux-musl-x64 linux-arm64 linux-arm osx-x64 osx-arm64 win-x64 win-arm64"

# Determine which runtimes to build
if [[ -z "$RUNTIMES_ARG" ]]; then
  # Default: detect current container runtime
  machine_arch=$(uname -m)
  case "$machine_arch" in
    x86_64)  runtimes="linux-x64" ;;
    aarch64) runtimes="linux-arm64" ;;
    *)
      echo "Unsupported machine architecture: $machine_arch"
      exit 1
      ;;
  esac
  echo "Building for detected runtime: $runtimes"
elif [[ "$RUNTIMES_ARG" == "all" ]]; then
  runtimes="$ALL_RUNTIMES"
  echo "Building for all supported runtimes"
  GZIP_OUTPUT=true  # Always gzip when building all
else
  # Parse comma-separated list
  runtimes="${RUNTIMES_ARG//,/ }"
  echo "Building for specified runtimes: $runtimes"
fi

# Ensure tree-sitter-darklang libraries exist for the runtimes we're building
if [[ "$runtimes" == "$ALL_RUNTIMES" ]]; then
  ./scripts/build/build-tree-sitter-darklang.sh --runtimes=all
else
  ./scripts/build/build-tree-sitter-darklang.sh --runtimes="${runtimes// /,}"
fi

# AOT-published CLIs statically link libe_sqlite3 (DirectPInvoke binding in
# Cli.fsproj), so the per-RID archives must exist before `dotnet publish`.
# JIT publish dynamically loads the .so from the NuGet, so this step is a
# no-op for non-AOT builds. We filter win-* out: build-sqlite.sh doesn't
# produce windows archives yet (and Cli.fsproj has no <NativeLibrary>
# items for win-x64/win-arm64), so any AOT-win build would fail at link
# time regardless. Caller is expected to skip win runtimes when --aot.
if [[ "$AOT" == "true" ]]; then
  sqlite_runtimes=""
  for rt in $runtimes; do
    if [[ "$rt" != win-* ]]; then
      sqlite_runtimes+="${sqlite_runtimes:+,}$rt"
    fi
  done
  if [[ -n "$sqlite_runtimes" ]]; then
    ./scripts/build/build-sqlite.sh --runtimes="$sqlite_runtimes"
  fi
fi

# Build function
build_for_runtime() {
  local rt="$1"
  echo "Building for runtime: $rt"

  if [[ "$AOT" == "true" ]]; then
    # AOT mode: native compilation, no PublishReadyToRun, no PublishSingleFile
    # (AOT already produces a single native binary).
    ./scripts/build/_dotnet-wrapper publish \
      -c Release \
      src/Cli/Cli.fsproj \
      /p:DebugType=None \
      /p:DebugSymbols=false \
      /p:PublishAot=true \
      --self-contained true \
      --runtime "$rt"
  else
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
  fi

  target="clis/darklang-$release-$rt"
  echo "Moving to $target"

  if [[ $rt == win-* ]]; then
    mv -f "backend/Build/out/Cli/Release/net10.0/$rt/publish/Cli.exe" "$target.exe"
    if [[ "$GZIP_OUTPUT" == "true" ]]; then
      gzip -f "$target.exe"
    fi
  else
    mv -f "backend/Build/out/Cli/Release/net10.0/$rt/publish/Cli" "$target"
    if [[ "$GZIP_OUTPUT" == "true" ]]; then
      gzip -f "$target"
    fi
  fi
}

# Build for each runtime
for rt in $runtimes; do
  build_for_runtime "$rt"
done

echo ""
echo "Build complete. Output in clis/"
ls -la clis/
