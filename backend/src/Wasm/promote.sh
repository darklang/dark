#!/usr/bin/env bash
# Promote the last publish to the directory the local server serves.
#
#   backend/src/Wasm/promote.sh
#
# `dotnet publish` writes rundir/wasm-repl, and a publish starts by wiping it, so serving
# that directory means the site is down for the length of every build. The server serves
# rundir/wasm-site instead, and this copies a finished publish into it and swaps it in (so the
# old files stay until the new ones are all there). Run it after a publish
# that has been checked, not before.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
SRC="$ROOT/rundir/wasm-repl/wwwroot"
DST="$ROOT/rundir/wasm-site/wwwroot"
[[ -f "$SRC/cli.html" && -d "$SRC/_framework" ]] || { echo "no publish at $SRC" >&2; exit 1; }
"$ROOT/backend/src/Wasm/make-store.sh" "$SRC/data.db" >/dev/null
# The page fetches data.db.br and inflates it itself (see Host/Cli.fs Boot). Brotli with a
# 16 MB window is less than half of gzip on this file: the hex hashes repeat a lot.
python3 -c "import brotli,sys; open(sys.argv[1]+'.br','wb').write(brotli.compress(open(sys.argv[1],'rb').read(), quality=11, lgwin=24))" "$SRC/data.db"
# What the page fetches, and how big it inflates to.
printf '{"file":"data.db.br","size":%s}\n' "$(stat -c %s "$SRC/data.db")" > "$SRC/store.json"
mkdir -p "$DST"
# The page sources are copied straight from the tree so an html/js edit needs no publish.
cp "$ROOT/backend/src/Wasm/wwwroot/"*.html "$SRC/"
rm -f "$SRC"/*.html.gz "$SRC"/*.html.br
# Into place under the old tree, then swap, so a page being served never sees a half-copy.
rm -rf "$DST.next"
cp -r "$SRC" "$DST.next"
find "$DST.next" -name '*.pdb' -delete
rm -rf "$DST.old"; [[ -d "$DST" ]] && mv "$DST" "$DST.old"; mv "$DST.next" "$DST"; rm -rf "$DST.old"
echo "promoted to $DST ($(du -sh "$DST" | cut -f1))"
