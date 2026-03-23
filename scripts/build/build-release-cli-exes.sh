#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Builds the `darklang` CLI exe(s) for specified or default runtime(s).
#
# Usage:
#   ./scripts/build/build-release-cli-exes.sh                    # Build for current container runtime
#   ./scripts/build/build-release-cli-exes.sh --runtimes=all     # Build for all supported runtimes
#   ./scripts/build/build-release-cli-exes.sh --runtimes=linux-x64,osx-arm64  # Build for specific runtimes
#
# Supported runtimes: linux-x64, linux-musl-x64, linux-arm64, linux-arm, osx-x64, osx-arm64, win-x64, win-arm64

set -euo pipefail

# Parse arguments
RUNTIMES_ARG=""
GZIP_OUTPUT=false

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

# Force WAL checkpoint to ensure all data is committed to main DB before embedding
echo "Forcing WAL checkpoint to commit all data before embedding database..."
sqlite3 rundir/data.db "PRAGMA wal_checkpoint(TRUNCATE);" || echo "WAL checkpoint completed (or no WAL file exists)"

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

# Build function
build_for_runtime() {
  local rt="$1"
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
    if [[ "$GZIP_OUTPUT" == "true" ]]; then
      gzip -f "$target.exe"
    fi
  else
    mv -f "backend/Build/out/Cli/Release/net8.0/$rt/publish/Cli" "$target"
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
