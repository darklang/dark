#!/bin/bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# This builds the tree-sitter-darklang grammar library for specified platforms.
#
# Usage:
#   ./scripts/build/build-tree-sitter-darklang.sh                    # Build for current platform only
#   ./scripts/build/build-tree-sitter-darklang.sh --runtimes=all     # Build for all platforms
#   ./scripts/build/build-tree-sitter-darklang.sh --runtimes=linux-x64,osx-arm64  # Build specific
#
# The grammar is compiled from tree-sitter-darklang/src/parser.c and scanner.c,
# producing shared libraries that are embedded in our CLI executables.
#
# Note: The list of build targets should be updated _in conjunction with_ equivalent
# updates in `LibTreeSitter.fsproj`, as well as `build-release-cli-exes.sh`.

set -euo pipefail

# Parse arguments
RUNTIMES_ARG=""
for arg in "$@"; do
  case $arg in
    --runtimes=*)
      RUNTIMES_ARG="${arg#*=}"
      ;;
  esac
done

cd ~/app

# Source and output directories
grammar_dir="tree-sitter-darklang/src"
output_dir="backend/src/LibTreeSitter/lib"

# Source files for the darklang grammar
grammar_sources="$grammar_dir/parser.c $grammar_dir/scanner.c"
include_flags="-I $grammar_dir"

mkdir -p "$output_dir"

# Map runtime to library filename and zig target
get_lib_info() {
  local rt="$1"
  case "$rt" in
    linux-x64)      echo "tree-sitter-darklang-linux-x64.so x86_64-linux-gnu" ;;
    linux-musl-x64) echo "tree-sitter-darklang-linux-musl-x64.so x86_64-linux-musl" ;;
    linux-arm64)    echo "tree-sitter-darklang-linux-arm64.so aarch64-linux-gnu" ;;
    linux-arm)      echo "tree-sitter-darklang-linux-arm.so arm-linux-gnueabihf" ;;
    osx-x64)        echo "tree-sitter-darklang-macos-x64.dylib x86_64-macos-none" ;;
    osx-arm64)      echo "tree-sitter-darklang-macos-arm64.dylib aarch64-macos-none" ;;
    win-x64)        echo "tree-sitter-darklang-windows-x64.dll x86_64-windows-gnu" ;;
    win-arm64)      echo "tree-sitter-darklang-windows-arm64.dll aarch64-windows-gnu" ;;
    *)              echo "" ;;
  esac
}

build_for_runtime() {
  local rt="$1"
  local info
  info=$(get_lib_info "$rt")

  if [[ -z "$info" ]]; then
    echo "Unknown runtime: $rt"
    return 1
  fi

  local lib_name zig_target
  lib_name=$(echo "$info" | cut -d' ' -f1)
  zig_target=$(echo "$info" | cut -d' ' -f2)

  local output_path="$output_dir/$lib_name"

  if [[ -f "$output_path" ]]; then
    echo "Already exists: $lib_name"
    return 0
  fi

  echo "Building $lib_name..."
  $HOME/zig/zig cc -target "$zig_target" -fPIC -shared -o "$output_path" $grammar_sources $include_flags
}

build_default() {
  local output_path="$output_dir/tree-sitter-darklang.so"
  if [[ -f "$output_path" ]]; then
    echo "Already exists: tree-sitter-darklang.so"
    return 0
  fi
  echo "Building tree-sitter-darklang.so (default platform)..."
  $HOME/zig/zig cc -fPIC -shared -o "$output_path" $grammar_sources $include_flags
}

ALL_RUNTIMES="linux-x64 linux-musl-x64 linux-arm64 linux-arm osx-x64 osx-arm64 win-x64 win-arm64"

if [[ -z "$RUNTIMES_ARG" ]]; then
  # Default: just build for current platform
  build_default
elif [[ "$RUNTIMES_ARG" == "all" ]]; then
  echo "Building tree-sitter-darklang for all platforms..."
  build_default
  for rt in $ALL_RUNTIMES; do
    build_for_runtime "$rt"
  done
else
  # Build for specified runtimes
  runtimes="${RUNTIMES_ARG//,/ }"
  echo "Building tree-sitter-darklang for: $runtimes"
  for rt in $runtimes; do
    build_for_runtime "$rt"
  done
fi

echo ""
echo "Done. Libraries in $output_dir:"
ls -la "$output_dir"/tree-sitter-darklang* 2>/dev/null || echo "(none built yet)"
