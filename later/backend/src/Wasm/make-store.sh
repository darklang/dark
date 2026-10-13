#!/usr/bin/env bash
# Produce the package store the browser CLI boots from: a copy of THIS clone's
# rundir/data.db with every per-instance setting removed, then checked.
#
#   backend/src/Wasm/make-store.sh [out]      default: rundir/wasm-repl/wwwroot/data.db
#
# What gets stripped, and why it must be:
#   config_v0        holds `sync.relay`, the push cursors and `sync.secret.<url>`, the write
#                    secret shared between a person's machines and production. It leaves in
#                    a copy, so it is deleted here, then asserted gone, then the raw bytes are
#                    scanned as well. A store that fails any check is not written.
#   cli-config.json  never copied (instance id/name, session account)
#   ~/.darklang      never read. The source is always the clone's own rundir.
#   traces           `traces` and `trace_fn_calls` are the clone's own recorded executions,
#                    captured Dvals and all. Nothing in the browser reads them, and they are
#                    what mints most of the orphan blobs the sweep below reclaims.
#
# Also: journal_mode back to DELETE (WAL needs shared memory the browser's in-memory
# filesystem does not have), a blob sweep, and VACUUM, which is what makes the file worth
# gzipping.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
SRC="$ROOT/rundir/data.db"
OUT="${1:-$ROOT/rundir/wasm-repl/wwwroot/data.db}"
# In `rundir`, not /tmp, so the sweep below can name it with a bare filename.
TMP="$(mktemp -p "$ROOT/rundir" wasm-store-XXXXXX.db)"
trap 'rm -f "$TMP" "$TMP-journal" "$TMP-wal" "$TMP-shm"' EXIT

[[ -f "$SRC" ]] || { echo "no store at $SRC (start the container and build first)" >&2; exit 1; }

# Checked here rather than at the end, because every expensive step is between the two and
# the failure arrived after all of them: the module is not in the image on any clone built
# before it was added, so the script did the whole job, wrote `data.db`, and then died
# without the `.br` the page actually fetches or the `store.json` that sizes it.
python3 -c "import brotli" 2>/dev/null || {
  echo "python3 cannot import brotli, which this script needs to write data.db.br." >&2
  echo "  Rebuild this clone's container (scripts/dev/start --recreate), which installs it," >&2
  echo "  or for a one-off: pip install brotli" >&2
  exit 1
}

# A consistent snapshot even if the store is open in WAL mode elsewhere.
sqlite3 "$SRC" "VACUUM INTO '$TMP'"

sqlite3 "$TMP" <<'SQL'
DELETE FROM config_v0;
DELETE FROM sync_pushed;
DELETE FROM sync_bases;
DELETE FROM relay_branches;
SQL

# Every `trace%` table, FOUND rather than listed. This was a list of two, and two more
# arrived after it was written (`trace_fns`, `trace_loops`), so the browser has been
# getting them ever since -- orphaned, because the traces they belong to were deleted
# here and they were not. Nothing in the browser reads any trace table, so matching the
# prefix is safe, and unlike a list it cannot go stale when the next one is added.
traceTables=$(sqlite3 "$TMP" \
  "SELECT name FROM sqlite_master WHERE type = 'table' AND name LIKE 'trace%' ORDER BY name")
[[ -n "$traceTables" ]] || { echo "no trace tables in the store at all; refusing" >&2; exit 1; }
while read -r t; do
  sqlite3 "$TMP" "DELETE FROM \"$t\""
done <<< "$traceTables"
echo "cleared $(wc -l <<< "$traceTables") trace table(s): $(tr '\n' ' ' <<< "$traceTables")" >&2

# An orphan `package_blobs` row -- one whose hash no `package_values.rt_dval` references any
# more -- is a LIVE row, so no VACUUM touches it. They arrive on their own: every trace
# capture promotes its ephemeral blobs into the table, and a package reload leaves behind the
# blobs of every value it replaced. On a clone that has been worked in this is the largest
# table in the store by a wide margin.
#
# Pointed at the SNAPSHOT, not at the clone's own store, through DARK_CONFIG_DB_NAME: a bare
# filename resolved against `rundir`, so there is no host path to carry across the container
# hop that `run-local-exec` may make.
#
# Order matters. `Blob.sweepOrphans` scans `package_values` and nothing else, so a blob held
# only by a trace or by a User DB row looks orphaned to it. The trace deletes above are what
# make its answer right; `user_data_v0` rows belong to whoever put them there, so skip the
# sweep rather than delete them.
userRows=$(sqlite3 "$TMP" "SELECT count(*) FROM user_data_v0")
if [[ "$userRows" == "0" ]]; then
  (cd "$ROOT" && DARK_CONFIG_DB_NAME="$(basename "$TMP")" ./scripts/run-local-exec pm-sweep-blobs)
else
  echo "user_data_v0 has $userRows row(s); skipping the blob sweep, which only sees package_values" >&2
fi

# After the sweep, which reopens the file in WAL and leaves pages free.
sqlite3 "$TMP" <<'SQL'
PRAGMA journal_mode=DELETE;
VACUUM;
SQL

# Assert, don't assume. Each check says how many things it LOOKED at, so a check that
# examined nothing cannot read as a check that found nothing wrong.
left=$(sqlite3 "$TMP" "SELECT count(*) FROM config_v0")
[[ "$left" == "0" ]] || { echo "config_v0 still has $left rows; refusing" >&2; exit 1; }
while read -r t; do
  n=$(sqlite3 "$TMP" "SELECT count(*) FROM \"$t\"")
  [[ "$n" == "0" ]] || { echo "$t still has $n row(s) after the delete; refusing" >&2; exit 1; }
done <<< "$traceTables"
echo "checked $(wc -l <<< "$traceTables") trace table(s), all empty" >&2
# The key names appear as string literals in the packaged code, so look for a stored KEY,
# which is the name followed by the relay url: `sync.secret.http...`.
if LC_ALL=C grep -a -q "sync\.secret\.http" "$TMP"; then
  echo "the store bytes still carry a sync.secret.<url> key (free pages?); refusing" >&2
  exit 1
fi

mkdir -p "$(dirname "$OUT")"
mv "$TMP" "$OUT"
chmod 644 "$OUT" # mktemp made it 0600; the web server has to read it
# The page fetches data.db.br and inflates it itself (Host.fs, Boot); store.json carries the
# inflated size. Brotli with a 16 MB window is under half of gzip on this file.
python3 -c "import brotli,sys; open(sys.argv[1]+'.br','wb').write(brotli.compress(open(sys.argv[1],'rb').read(), quality=11, lgwin=24))" "$OUT"
# The script has never had an opinion on its own output. It printed the size and shipped
# whatever it got, which is how a store built from a clone somebody had worked in all day
# went out at 157 MB while the deployed one was 6.6 MB compressed. A ceiling turns that
# into a build failure that names the tables responsible.
maxMb="${WASM_STORE_MAX_MB:-80}"
outMb=$(( $(stat -c %s "$OUT") / 1024 / 1024 ))
if (( outMb > maxMb )); then
  {
    echo "the store is ${outMb} MB, over the ${maxMb} MB ceiling. Biggest tables:"
    sqlite3 "$OUT" "SELECT '    ' || name || '  ' || (SUM(pgsize) / 1024 / 1024) || ' MB'
                    FROM dbstat GROUP BY name ORDER BY SUM(pgsize) DESC LIMIT 6"
    echo "  Set WASM_STORE_MAX_MB higher if this is expected."
  } >&2
  exit 1
fi

printf '{"file":"data.db.br","size":%s}\n' "$(stat -c %s "$OUT")" > "$(dirname "$OUT")/store.json"
echo "wrote $OUT ($(du -h "$OUT" | cut -f1)) and .br; config_v0 empty, no sync credentials in bytes"
