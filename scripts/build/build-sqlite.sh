#!/bin/bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Builds SQLite as a static archive (libe_sqlite3-<rid>.a) per target runtime.
#
# Consumed by the NativeAOT-published CLI: backend/src/Cli/Cli.fsproj wires
# these archives via <DirectPInvoke Include="e_sqlite3" /> +
# <NativeLibrary Include="lib/libe_sqlite3-$(RuntimeIdentifier).a" />, so the
# AOT-published binary has all SQLite symbols statically resolved at link time
# and doesn't need libe_sqlite3.so on the user's filesystem.
#
# The DllImport name in SQLitePCLRaw.provider.e_sqlite3 is "e_sqlite3"; the
# archive base name matches that so DirectPInvoke binds the import. The .a
# suffix is by Linux/Apple convention — Windows static libs would be .lib,
# but we don't currently cross-compile the windows variants from this script
# (the CLI's windows AOT build path is not active yet; adding it would mean
# adding win-x64/win-arm64 to the runtime map below AND adding matching
# <NativeLibrary> ItemGroups to Cli.fsproj).
#
# Usage:
#   ./scripts/build/build-sqlite.sh                                # current arch only
#   ./scripts/build/build-sqlite.sh --runtimes=all                 # full cross-compile matrix
#   ./scripts/build/build-sqlite.sh --runtimes=linux-x64,osx-arm64 # specified subset
#
# Note: the list of runtime targets here must stay in sync with
# - backend/src/Cli/Cli.fsproj (per-RID <NativeLibrary> items)
# - scripts/build/build-release-cli-exes.sh
# - backend/src/LibTreeSitter/LibTreeSitter.fsproj
#
# Uses zig cc for cross-compile (single toolchain, no per-target sysroot
# wrangling), matching the pattern in build-tree-sitter-darklang.sh.

set -euo pipefail

# Parse arguments
RUNTIMES_ARG=""
for arg in "$@"; do
  case $arg in
    --runtimes=*)
      RUNTIMES_ARG="${arg#*=}"
      ;;
    *)
      echo "Unknown argument: $arg"
      exit 1
      ;;
  esac
done

cd ~/

# SQLite amalgamation (single-file C source). Public domain. The version pin
# is intentional — bumping it changes the on-disk database format only across
# major versions, and we want reproducible builds.
sqlite_version="3460000"
sqlite_url="https://www.sqlite.org/2024/sqlite-amalgamation-${sqlite_version}.zip"
sqlite_sha256="712a7d09d2a22652fb06a49af516e051979a3984adb067da86760e60ed51a7f5"
sqlite_dir="sqlite-amalgamation-${sqlite_version}"

output_base_dir="app/backend/src/Cli/lib"

# Map a .NET runtime identifier to its zig cross-compile target triple. Add
# a case here (and a matching <NativeLibrary> in Cli.fsproj) when expanding
# the supported AOT target set.
get_zig_target() {
  case "$1" in
    linux-x64)      echo "x86_64-linux-gnu" ;;
    linux-musl-x64) echo "x86_64-linux-musl" ;;
    linux-arm64)    echo "aarch64-linux-gnu" ;;
    linux-arm)      echo "arm-linux-gnueabihf" ;;
    osx-x64)        echo "x86_64-macos-none" ;;
    osx-arm64)      echo "aarch64-macos-none" ;;
    *)              echo "" ;;
  esac
}

ALL_RUNTIMES="linux-x64 linux-musl-x64 linux-arm64 linux-arm osx-x64 osx-arm64"

# Detect the host's runtime identifier — used as the local-dev default so a
# fresh checkout doesn't burn six cross-compiles for archives nobody will
# load. Caller can opt into the full matrix with --runtimes=all.
detect_host_rid() {
  local kernel arch
  kernel=$(uname -s)
  arch=$(uname -m)
  case "$kernel-$arch" in
    Linux-x86_64)   echo "linux-x64" ;;
    Linux-aarch64)  echo "linux-arm64" ;;
    Linux-armv7l)   echo "linux-arm" ;;
    Darwin-x86_64)  echo "osx-x64" ;;
    Darwin-arm64)   echo "osx-arm64" ;;
    *)              echo "" ;;
  esac
}

# Resolve which runtimes to build
if [[ -z "$RUNTIMES_ARG" ]]; then
  host_rid=$(detect_host_rid)
  if [[ -z "$host_rid" ]]; then
    echo "Could not detect host runtime identifier ($(uname -s)-$(uname -m))."
    echo "Pass --runtimes=<rid[,rid...]> or --runtimes=all explicitly."
    exit 1
  fi
  runtimes="$host_rid"
  echo "Building SQLite for detected host runtime: $runtimes"
elif [[ "$RUNTIMES_ARG" == "all" ]]; then
  runtimes="$ALL_RUNTIMES"
  echo "Building SQLite for all supported runtimes"
else
  runtimes="${RUNTIMES_ARG//,/ }"
  echo "Building SQLite for specified runtimes: $runtimes"
fi

# Skip if every requested artifact already exists. Per-rid cache check, so
# a host-only run doesn't re-trigger because the cross-compile artifacts
# are missing.
needs_build=false
for rid in $runtimes; do
  if [[ ! -f "$output_base_dir/libe_sqlite3-${rid}.a" ]]; then
    needs_build=true
    break
  fi
done
if [[ "$needs_build" == "false" ]]; then
  echo "All requested SQLite archives already present in $output_base_dir. Skipping."
  exit 0
fi

# Fetch + verify amalgamation
if [[ ! -d "$sqlite_dir" ]]; then
  curl -sSL -O "$sqlite_url"
  echo "$sqlite_sha256  sqlite-amalgamation-${sqlite_version}.zip" | sha256sum -c -
  unzip -q "sqlite-amalgamation-${sqlite_version}.zip"
fi

mkdir -p "$output_base_dir"

# Compile flags. We need to satisfy the symbol set that
# SQLitePCLRaw.provider.e_sqlite3 imports via [DllImport("e_sqlite3")] —
# under NativeAOT + DirectPInvoke those become hard link references, so any
# DllImport whose target is missing breaks the link even if F# never calls
# it. SQLitePCL's bindings include load_extension and the column-metadata
# functions unconditionally, so we have to keep them in the build.
#
# Notable choices:
#   ENABLE_COLUMN_METADATA — needed for sqlite3_column_{database,origin,table}_name
#                            which the provider binds unconditionally.
#   load extension is enabled at COMPILE time (symbols present) but disabled
#     at RUNTIME by default; we never call sqlite3_enable_load_extension(db,1),
#     so we get the security posture for free.
#   THREADSAFE=1 — full mutex, matches what SQLitePCLRaw ships.
#   DQS=0 — reject double-quoted string literals, catches SQL bugs early.
#   DEFAULT_MEMSTATUS=0 — drops per-connection memstats overhead.
#   DO NOT add SQLITE_OMIT_AUTOINIT — SQLitePCLRaw's provider doesn't call
#     sqlite3_initialize() explicitly; it relies on SQLite's default
#     auto-init-on-first-API-call. With OMIT_AUTOINIT we get a segfault on
#     the first sqlite3_open call. Tested 2026-05-12.
sqlite_cflags="-Os \
  -DSQLITE_THREADSAFE=1 \
  -DSQLITE_ENABLE_COLUMN_METADATA \
  -DSQLITE_ENABLE_MATH_FUNCTIONS \
  -DSQLITE_DEFAULT_MEMSTATUS=0 \
  -DSQLITE_DQS=0"

sqlite_src="$sqlite_dir/sqlite3.c"
sqlite_inc="-I $sqlite_dir"

# Build one static archive for the given .NET runtime identifier.
# Two-step: zig cc -c → .o, then zig ar rcs → .a. Zig bundles llvm-ar so we
# get target-correct archives regardless of host.
build_one() {
  local rid="$1"
  local zig_target
  zig_target=$(get_zig_target "$rid")
  if [[ -z "$zig_target" ]]; then
    echo "Unknown runtime identifier: $rid"
    return 1
  fi

  local arfile="$output_base_dir/libe_sqlite3-${rid}.a"
  if [[ -f "$arfile" ]]; then
    echo "  ✓ $rid (cached)"
    return 0
  fi

  local objfile="$output_base_dir/sqlite3-${rid}.o"
  echo "  → $rid"
  "$HOME/zig/zig" cc -target "$zig_target" $sqlite_cflags -fPIC -c \
    $sqlite_inc "$sqlite_src" -o "$objfile"
  "$HOME/zig/zig" ar rcs "$arfile" "$objfile"
  rm -f "$objfile"
}

echo "Compiling SQLite static archives via zig cc..."

# Run builds in parallel and surface any failures. `set -e` + `&` does not
# propagate child exit codes through `wait` on its own — we track pids and
# check each one so a single broken cross-compile fails the script.
pids=()
for rid in $runtimes; do
  build_one "$rid" &
  pids+=("$!:$rid")
done

failed=0
for entry in "${pids[@]}"; do
  pid="${entry%%:*}"
  rid="${entry##*:}"
  if ! wait "$pid"; then
    echo "Build failed for $rid"
    failed=1
  fi
done
if [[ $failed -ne 0 ]]; then
  exit 1
fi

# Clean up amalgamation source (we re-download next time; keeps the repo
# tree clean and avoids accidentally committing it).
rm -rf "$sqlite_dir" "sqlite-amalgamation-${sqlite_version}.zip"

echo ""
echo "Done. Artifacts:"
ls -lh "$output_base_dir"/libe_sqlite3-*.a
