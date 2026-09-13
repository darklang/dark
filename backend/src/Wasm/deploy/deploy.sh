#!/usr/bin/env bash
# Deploy the published site to fly.io as `dark-wasm`.
#
#   backend/src/Wasm/deploy/deploy.sh
#
# Expects rundir/wasm-repl/wwwroot to exist (dotnet publish) and REBUILDS the store with
# make-store.sh so what ships is the sanitized one, never a hand-copied file. Stages the
# site into a throwaway build context: the Dockerfile copies `site/` and nothing else.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../../.." && pwd)"
# Ship what the local server serves (the promoted, checked build), not a publish in progress.
SITE="$ROOT/rundir/wasm-site/wwwroot"
[[ -f "$SITE/cli.html" && -d "$SITE/_framework" ]] || { echo "no published site at $SITE" >&2; exit 1; }

"$ROOT/backend/src/Wasm/make-store.sh" "$SITE/data.db"
# The secret check, once more, on the exact bytes that ship.
if LC_ALL=C grep -a -q "sync\.secret\.http" "$SITE/data.db"; then
  echo "refusing to deploy: the store carries a sync secret key" >&2; exit 1
fi

CTX="$(mktemp -d)"
trap 'rm -rf "$CTX"' EXIT
cp "$HERE/Dockerfile" "$HERE/nginx.conf" "$HERE/fly.toml" "$CTX/"
mkdir -p "$CTX/site"
# The site minus symbols, brotli variants (stock nginx has no brotli) and stale gzips; then
# gzip everything worth it so gzip_static has a fresh .gz beside each file.
rsync -a --exclude '*.pdb' --exclude '*.br' --exclude '*.gz' "$SITE/" "$CTX/site/"
find "$CTX/site" -type f \( -name '*.wasm' -o -name '*.js' -o -name '*.json' -o -name '*.html' \
  -o -name '*.css' -o -name '*.db' -o -name '*.dat' -o -name '*.snapshot' \) -size +1k \
  -exec gzip -k -f -6 {} +
# The page fetches data.db.gz by name; the raw store need not ship at all.
rm -f "$CTX/site/data.db"
du -sh "$CTX/site"
cd "$CTX"
fly deploy --remote-only --config fly.toml "$@"
