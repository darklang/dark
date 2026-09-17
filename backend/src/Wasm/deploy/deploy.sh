#!/usr/bin/env bash
# Deploy the site to fly.io as `dark-wasm`.
#
#   backend/src/Wasm/deploy/deploy.sh [fly deploy args]
#
# Ships rundir/wasm-site/wwwroot, which promote.sh produced from a publish (and which built
# the store through make-store.sh, the step that strips the sync credentials). The store's
# bytes are checked once more here, on exactly what leaves the machine. Needs only bash,
# gzip and flyctl (FLY_API_TOKEN in the environment, or a logged-in fly; CI maps WASM_FLY_API_TOKEN to it), so CI can run it
# on a plain image.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../../.." && pwd)"
SITE="$ROOT/rundir/wasm-site/wwwroot"
[[ -f "$SITE/cli.html" && -d "$SITE/_framework" && -f "$SITE/data.db.br" ]] \
  || { echo "no promoted site at $SITE; run promote.sh first" >&2; exit 1; }
if LC_ALL=C grep -a -q "sync\.secret\.http" "$SITE/data.db"; then
  echo "refusing to deploy: the store carries a sync secret key" >&2; exit 1
fi

CTX="$(mktemp -d)"
trap 'rm -rf "$CTX"' EXIT
cp "$HERE/Dockerfile" "$HERE/nginx.conf" "$HERE/fly.toml" "$CTX/"
mkdir -p "$CTX/site"
# The site minus symbols, the publish's brotli variants (stock nginx has no brotli module),
# stale gzips, and the raw store (the page fetches data.db.br). Then gzip everything worth it
# so gzip_static has a fresh .gz beside each file.
cp -r "$SITE/." "$CTX/site/"
find "$CTX/site" \( -name '*.pdb' -o -name '*.gz' -o -path '*/_framework/*.br' \) -delete
rm -f "$CTX/site/data.db"
find "$CTX/site" -type f \( -name '*.wasm' -o -name '*.js' -o -name '*.json' -o -name '*.html' \
  -o -name '*.css' -o -name '*.dat' -o -name '*.snapshot' \) -size +1k -exec gzip -k -f -6 {} +
du -sh "$CTX/site"
cd "$CTX"
fly deploy --remote-only --config fly.toml "$@"
