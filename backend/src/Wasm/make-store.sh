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
#
# Also: journal_mode back to DELETE (WAL needs shared memory the browser's in-memory
# filesystem does not have) and VACUUM, which is what makes the file worth gzipping.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
SRC="$ROOT/rundir/data.db"
OUT="${1:-$ROOT/rundir/wasm-repl/wwwroot/data.db}"
TMP="$(mktemp --suffix=.db)"
trap 'rm -f "$TMP" "$TMP-journal" "$TMP-wal" "$TMP-shm"' EXIT

[[ -f "$SRC" ]] || { echo "no store at $SRC (start the container and build first)" >&2; exit 1; }

# A consistent snapshot even if the store is open in WAL mode elsewhere.
sqlite3 "$SRC" "VACUUM INTO '$TMP'"

sqlite3 "$TMP" <<'SQL'
DELETE FROM config_v0;
DELETE FROM sync_pushed;
DELETE FROM sync_bases;
DELETE FROM relay_branches;
PRAGMA journal_mode=DELETE;
VACUUM;
SQL

# Assert, don't assume.
left=$(sqlite3 "$TMP" "SELECT count(*) FROM config_v0")
[[ "$left" == "0" ]] || { echo "config_v0 still has $left rows; refusing" >&2; exit 1; }
# The key names appear as string literals in the packaged code, so look for a stored KEY,
# which is the name followed by the relay url: `sync.secret.http...`.
if LC_ALL=C grep -a -q "sync\.secret\.http" "$TMP"; then
  echo "the store bytes still carry a sync.secret.<url> key (free pages?); refusing" >&2
  exit 1
fi

mkdir -p "$(dirname "$OUT")"
mv "$TMP" "$OUT"
chmod 644 "$OUT" # mktemp made it 0600; the web server has to read it
echo "wrote $OUT ($(du -h "$OUT" | cut -f1)); config_v0 empty, no sync credentials in bytes"
