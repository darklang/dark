#!/usr/bin/env bash
# Build the whole browser site: publish, then the store. From the repo root, inside
# the container. `deploy/deploy.sh` ships what this leaves in `rundir/wasm-repl/wwwroot`.
#
#   backend/src/Wasm/build.sh              everything
#   backend/src/Wasm/build.sh --store-only  just the store, when only packages changed
#
# This exists because the three steps had to be run by hand in the right order, and two of
# them have a way to go wrong that costs more than the build does:
#
#   - a bare `dotnet publish` piped anywhere HANGS, apparently forever. MSBuild spawns
#     worker nodes with node reuse on; they inherit stdout and outlive the parent, so the
#     pipe never closes and whatever dotnet printed sits unread in its buffer. Measured once
#     at 40 minutes of nothing. Node reuse is off here and output goes to a file.
#   - the publish does not clean up after itself, so stale fingerprinted files pile up in
#     the output directory and get deployed alongside the current ones. The wipe below was
#     a line in the README that a person had to remember.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT"

storeOnly=false
[[ "${1:-}" == "--store-only" ]] && storeOnly=true

OUT="rundir/wasm-repl"
LOG="rundir/logs/wasm-publish.log"

if [[ "$storeOnly" == false ]]; then
  command -v dotnet >/dev/null || { echo "no dotnet on PATH; are you inside the container?" >&2; exit 1; }
  backend/src/Wasm/setup.sh

  echo "publishing to $OUT (Release, AOT; 4 to 8 minutes). Follow along: tail -F $LOG"
  rm -rf "$OUT"
  mkdir -p "$(dirname "$LOG")"

  # Node reuse off, output to a file. See the note above: piping this is what hangs.
  if ! MSBUILDDISABLENODEREUSE=1 DOTNET_CLI_USE_MSBUILD_SERVER=0 dotnet publish \
       backend/src/Wasm/Wasm.csproj -c Release -o "$OUT" -nodeReuse:false > "$LOG" 2>&1; then
    echo "the publish failed. Last lines of $LOG:" >&2
    tail -25 "$LOG" >&2
    exit 1
  fi

  # A publish that "succeeded" without producing the site is the failure that reads as a
  # pass, so name the artifact rather than trusting the exit code.
  [[ -f "$OUT/wwwroot/cli.html" ]] || {
    echo "the publish exited 0 but $OUT/wwwroot/cli.html is not there; see $LOG" >&2
    exit 1
  }
  echo "published: $(du -sh "$OUT/wwwroot" | cut -f1) in $OUT/wwwroot"
fi

backend/src/Wasm/make-store.sh

echo
echo "the site is in $OUT/wwwroot:"
for f in cli.html index.html eval.html worker.js data.db data.db.br store.json; do
  if [[ -e "$OUT/wwwroot/$f" ]]; then
    printf '  %-14s %s\n' "$f" "$(du -h "$OUT/wwwroot/$f" | cut -f1)"
  else
    printf '  %-14s MISSING\n' "$f"
  fi
done
# What a cold visit costs. nginx serves the framework's own `.br` beside each file to a
# browser (`brotli_static`), so this counts what that server would actually send: the brotli
# framework and the brotli store. An upper bound, since the runtime fetches the
# assemblies its boot set names rather than all of them.
fwBr=$(find "$OUT/wwwroot/_framework" -type f -name '*.br' -printf '%s\n' 2>/dev/null | awk '{s+=$1} END {print s+0}')
fwJs=0
dbBr=$(stat -c %s "$OUT/wwwroot/data.db.br" 2>/dev/null || echo 0)
echo
awk -v fw="$fwBr" -v js="$fwJs" -v db="$dbBr" 'BEGIN {
  printf "cold load, upper bound: %.1f MB\n", (fw + js + db) / 1048576
  printf "  runtime and assemblies  %6.1f MB (brotli)\n", (fw + js) / 1048576
  printf "  the package store       %6.1f MB (data.db.br)\n", db / 1048576
}'

echo
echo "serve it:   python3 -m http.server 9090 -d $OUT/wwwroot"
echo "deploy it:  backend/src/Wasm/deploy/deploy.sh"
