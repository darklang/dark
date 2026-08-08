#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# Builds the `darklang` CLI exe(s) for specified or default runtime(s).
#
# Usage:
#   ./scripts/build/build-release-cli-exes.sh                    # Build for current container runtime
#   ./scripts/build/build-release-cli-exes.sh --runtimes=all     # Build for all supported runtimes
#   ./scripts/build/build-release-cli-exes.sh --runtimes=linux-x64,osx-arm64  # Build for specific runtimes
#   ./scripts/build/build-release-cli-exes.sh --mode=aot         # NativeAOT publish (see below)
#   ./scripts/build/build-release-cli-exes.sh --seed=path/to.db  # Use a pre-exported seed
#
# Supported runtimes: linux-x64, linux-musl-x64, linux-arm64, linux-arm, osx-x64, osx-arm64, win-x64, win-arm64
#
# Publish modes:
#   r2r    ReadyToRun + single-file + trimmed. What main ships today. Default.
#   aot    NativeAOT for every requested runtime. Errors out if any requested
#          runtime can't be AOT-built from this host (see the guards below).
#   auto   AOT where it's possible, R2R where it isn't. This is what the
#          release matrix uses: windows falls back without the caller
#          having to know why.
#
# NativeAOT runs ilc and then the *platform* linker, so a build host can only
# produce binaries for its own OS family. Cross-architecture within a family
# (an arm64 mac targeting osx-x64) is allowed and left to ilc to accept or
# reject; cross-OS is refused up front, because the failure is otherwise a
# confusing link error several minutes in.

set -euo pipefail

# Parse arguments
RUNTIMES_ARG=""
GZIP_OUTPUT=false
MODE="r2r"
SEED_PATH=""
DRY_RUN=false

for arg in "$@"; do
  case $arg in
    --dry-run)
      # Print the per-runtime publish plan and stop. Cheap way to check mode
      # selection, and what the guards would say, without a publish.
      DRY_RUN=true
      ;;
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
    --mode=*)
      MODE="${arg#*=}"
      ;;
    --aot)
      # Alias for --mode=aot. Kept because the notes and muscle memory use it.
      MODE="aot"
      ;;
    --seed=*)
      # Use an already-exported seed instead of exporting one here. CI exports
      # once in a dedicated job and hands the same bytes to every build job:
      # the export isn't safe against concurrent use of the store, and doing it
      # N times is N chances to embed a corrupt seed.
      SEED_PATH="${arg#*=}"
      ;;
    *)
      echo "Unknown argument: $arg"
      exit 1
      ;;
  esac
done

case "$MODE" in
  r2r|aot|auto) ;;
  *)
    echo "Unknown --mode=$MODE (expected r2r, aot or auto)"
    exit 1
    ;;
esac

sha=$(git rev-parse HEAD | cut -c 1-10)
release="alpha-$sha"

# Export GIT_COMMIT so the built binary includes the git hash
export GIT_COMMIT="$sha"

# All supported runtimes.
ALL_RUNTIMES="linux-x64 linux-musl-x64 linux-arm64 linux-arm osx-x64 osx-arm64 win-x64 win-arm64"

# Which runtimes we can NativeAOT at all. Windows is absent on purpose:
# build-sqlite.sh produces no windows archives and Cli.fsproj has no windows
# <NativeLibrary> items, so a win AOT publish fails at link. Adding windows
# means zig emitting .lib for both arches plus matching fsproj items.
#
# linux-arm (armv7) is absent too. .NET 10's 32-bit ARM NativeAOT runtime
# needs glibc symbols that only appeared in ~2.34, well above the floor we
# target, so it can't be linked here at all. That's a runtime requirement
# rather than a toolchain gap, so it isn't going to be fixed by better
# cross-compile flags. armv7 keeps the R2R build.
AOT_CAPABLE_RUNTIMES="linux-x64 linux-musl-x64 linux-arm64 osx-x64 osx-arm64"

# OS family of a runtime identifier, for the cross-OS guard.
rid_os_family() {
  case "$1" in
    linux-*) echo "linux" ;;
    osx-*)   echo "osx" ;;
    win-*)   echo "win" ;;
    *)       echo "" ;;
  esac
}

case "$(uname -s)" in
  Linux)  HOST_OS_FAMILY="linux" ;;
  Darwin) HOST_OS_FAMILY="osx" ;;
  *)
    echo "Unsupported build host: $(uname -s)"
    exit 1
    ;;
esac

# Can this host NativeAOT this runtime? Capability plus same-OS-family.
can_aot() {
  local rid="$1"
  case " $AOT_CAPABLE_RUNTIMES " in
    *" $rid "*) ;;
    *) return 1 ;;
  esac
  [[ "$(rid_os_family "$rid")" == "$HOST_OS_FAMILY" ]]
}

# Cross-architecture AOT needs an explicit linker toolchain. NativeAOT drives
# the final link through a C compiler, and the default gcc cannot cross-target
# at all: ilc passes `--target=<triple>` and gcc rejects it outright (measured
# 2026-08-06). clang takes --target, so a cross RID gets clang plus a sysroot.
#
# Same-arch builds are left alone: they link with the default toolchain, which
# is what the validated linux-x64 path already uses.
#
# The sysroot paths are what the cross packages in Dockerfile.aot-release install.
# A build host without those packages fails at link, which is the guard below.
cross_triple_for_runtime() {
  local rid="$1" host_arch
  host_arch=$(uname -m)
  case "$rid" in
    linux-arm64) [[ "$host_arch" == "aarch64" ]] || echo "aarch64-linux-gnu" ;;
    linux-arm)   [[ "$host_arch" == "armv7l" ]] || echo "arm-linux-gnueabihf" ;;
    *)           echo "" ;;
  esac
}

# The mode to actually publish a given runtime with.
mode_for_runtime() {
  local rid="$1"
  case "$MODE" in
    r2r) echo "r2r" ;;
    aot) echo "aot" ;;
    auto)
      if can_aot "$rid"; then echo "aot"; else echo "r2r"; fi
      ;;
  esac
}

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

# Guard: --mode=aot is a promise that every requested runtime gets AOT. If one
# of them can't, say so now and name the reason, rather than failing at link
# time ten minutes in. --mode=auto makes no such promise and skips this.
if [[ "$MODE" == "aot" ]]; then
  refused=""
  for rt in $runtimes; do
    if ! can_aot "$rt"; then
      case " $AOT_CAPABLE_RUNTIMES " in
        *" $rt "*)
          refused+="  $rt: needs a $(rid_os_family "$rt") build host (this one is $HOST_OS_FAMILY)"$'\n'
          ;;
        *)
          refused+="  $rt: not AOT-capable (no sqlite archive, no <NativeLibrary> in Cli.fsproj)"$'\n'
          ;;
      esac
    fi
  done
  if [[ -n "$refused" ]]; then
    echo "--mode=aot requested, but these runtimes can't be AOT-built here:"
    printf '%s' "$refused"
    echo "Use --mode=auto to fall back to R2R for these, or build them on the right host."
    exit 1
  fi
fi

# Report the plan before spending anything on it. --dry-run stops here, which
# is enough to check the mode selection without a publish.
echo "Publish plan:"
for rt in $runtimes; do
  echo "  $rt -> $(mode_for_runtime "$rt")"
done
if [[ "$DRY_RUN" == "true" ]]; then
  exit 0
fi

mkdir -p clis
rm -rf clis/.darklang

# Export a seed (smaller DB) and use it as the embedded data.db for smaller exes.
# The seed has full schema but no derived data — the grow step rebuilds on first run.
# We intentionally ship the slim seed (not a pre-projected DB) so every user
# has the same boot path and the ops are present for rewind/inspection.
#
# With --seed, we take someone else's export as-is. That's how the release
# matrix gets identical embedded bytes across every artifact, and how a build
# host with no LocalExec (a macOS runner) participates at all.
if [[ -n "$SEED_PATH" ]]; then
  if [[ ! -f "$SEED_PATH" ]]; then
    echo "--seed=$SEED_PATH does not exist"
    exit 1
  fi
  echo "Using pre-exported seed: $SEED_PATH"
  mkdir -p rundir
  cp "$SEED_PATH" rundir/data.db
else
  echo "Exporting seed for embedding..."
  sqlite3 rundir/data.db "PRAGMA wal_checkpoint(TRUNCATE);" || true
  scripts/run-local-exec export-seed rundir/seed.db
  cp rundir/seed.db rundir/data.db
fi
echo "Embedded data.db is the seed ($(du -h rundir/data.db | cut -f1))"

# AOT-published CLIs statically link libe_sqlite3 (DirectPInvoke binding in
# Cli.fsproj), so the per-RID archives must exist before `dotnet publish`.
# JIT publish dynamically loads the .so from the NuGet, so this step is a
# no-op for the R2R runtimes.
sqlite_runtimes=""
for rt in $runtimes; do
  if [[ "$(mode_for_runtime "$rt")" == "aot" ]]; then
    sqlite_runtimes+="${sqlite_runtimes:+,}$rt"
  fi
done
if [[ -n "$sqlite_runtimes" ]]; then
  ./scripts/build/build-sqlite.sh --runtimes="$sqlite_runtimes"
fi

# Build function
build_for_runtime() {
  local rt="$1"
  local rt_mode
  rt_mode="$(mode_for_runtime "$rt")"
  echo "Building for runtime: $rt (mode: $rt_mode)"

  if [[ "$rt_mode" == "aot" ]]; then
    # The archive has to be there before ilc, not discovered missing by the
    # linker. build-sqlite.sh above should have produced it; this catches a
    # stale cache restoring a partial lib/ directory, which is the realistic
    # way to get here.
    if [[ ! -f "backend/src/Cli/lib/libe_sqlite3-$rt.a" ]]; then
      echo "Missing backend/src/Cli/lib/libe_sqlite3-$rt.a, needed to AOT-link $rt."
      echo "Run: ./scripts/build/build-sqlite.sh --runtimes=$rt"
      exit 1
    fi

    # Cross-arch targets need clang and a sysroot; same-arch uses the default
    # toolchain. See cross_triple_for_runtime above for why.
    local cross_args=() triple
    triple="$(cross_triple_for_runtime "$rt")"
    if [[ -n "$triple" ]]; then
      if [[ ! -d "/usr/$triple" ]]; then
        echo "Cross-linking $rt needs a sysroot at /usr/$triple, which isn't here."
        echo "Build in the release image (Dockerfile.aot-release), which installs it."
        exit 1
      fi
      echo "  cross-linking via clang, sysroot /usr/$triple"
      # LinkerFlavor=lld matters as much as the sysroot: the default is the
      # host's ld.bfd, which fails with "unrecognised emulation mode" because
      # an x86_64 bfd can't emit for another architecture. lld cross-links
      # natively.
      # ObjCopyName is the third piece. After linking, ilc strips the binary
      # with objcopy, and the host x86_64 objcopy can't read an aarch64 file
      # ("Unable to recognise the format of the input file"). The cross
      # binutils ship a target-prefixed one.
      cross_args=(
        /p:CppCompilerAndLinker=clang
        "/p:SysRoot=/usr/$triple"
        /p:LinkerFlavor=lld
        "/p:ObjCopyName=$triple-objcopy"
      )
    fi

    # AOT mode: native compilation, no PublishReadyToRun, no PublishSingleFile
    # (AOT already produces a single native binary).
    ./scripts/build/_dotnet-wrapper publish \
      -c Release \
      src/Cli/Cli.fsproj \
      /p:DebugType=None \
      /p:DebugSymbols=false \
      /p:PublishAot=true \
      "${cross_args[@]}" \
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

  # Where the publish landed. Directory.Build.props computes DarkBuildDir
  # from its own location, but it's overridable, and a caller that overrides
  # it would otherwise watch this move fail on a path that no longer exists.
  local build_dir="${DarkBuildDir:-backend/Build}"
  local publish_dir="$build_dir/out/Cli/Release/net10.0/$rt/publish"

  target="clis/darklang-$release-$rt"
  echo "Moving to $target"

  if [[ $rt == win-* ]]; then
    mv -f "$publish_dir/Cli.exe" "$target.exe"
    if [[ "$GZIP_OUTPUT" == "true" ]]; then
      gzip -f "$target.exe"
    fi
  else
    mv -f "$publish_dir/Cli" "$target"
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
