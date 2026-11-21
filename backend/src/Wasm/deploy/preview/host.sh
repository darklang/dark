#!/usr/bin/env bash
# The preview host, from the outside: one fly app, one volume, a directory per site.
#
#   preview/host.sh upload <path> [site-dir]   put a published site at <path>
#   preview/host.sh remove <path>              take one off
#   preview/host.sh cleanup [--dry-run]        remove everything but latest and open PRs
#
# <path> is pulls/<n> or latest (the newest release; there is only one). site-dir
# defaults to rundir/wasm-repl/wwwroot after a publish and make-store.sh. Needs flyctl
# (logged in, or FLY_API_TOKEN), gzip, jq. PREVIEW_APP, PREVIEW_HOST, PREVIEW_REPO override
# the app, the hostname in the printed URL, and the repo whose PRs cleanup consults.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../../../.." && pwd)"
APP="${PREVIEW_APP:-dark-preview}"
HOST="${PREVIEW_HOST:-wasm.darklang.com}"
REPO="${PREVIEW_REPO:-darklang/dark}"

# Run a shell command on the machine, stdin attached. It stops when idle and ssh will not
# start it, so start it first.
on_host() {
  fly machines list -a "$APP" --json | jq -r '.[] | select(.state != "started") | .id' \
    | while read -r id; do [ -n "$id" ] && fly machines start "$id" -a "$APP" >/dev/null; done
  fly ssh console -a "$APP" --pty=false -C "sh -c \"$1\""
}
valid_path() { [[ "$1" =~ ^(pulls/[0-9]+|latest)$ ]] || { echo "path must be pulls/<n> or latest" >&2; exit 2; }; }

upload() {
  local path="${1:-}" site="${2:-$ROOT/rundir/wasm-repl/wwwroot}"
  valid_path "$path"
  [[ -f "$site/cli.html" && -d "$site/_framework" && -f "$site/data.db.br" ]] \
    || { echo "no site at $site; publish, then make-store.sh" >&2; exit 1; }

  # Stage: drop symbols, the publish's .br and stale .gz, and the raw store; gzip what is
  # worth it; then drop the _framework originals, since nginx serves the .gz regardless.
  local ctx; ctx="$(mktemp -d)"; trap "rm -rf '$ctx'" EXIT
  cp -r "$site/." "$ctx/site/"
  find "$ctx/site" \( -name '*.pdb' -o -name '*.gz' -o -path '*/_framework/*.br' \) -delete
  rm -f "$ctx/site/data.db"
  find "$ctx/site" -type f \( -name '*.wasm' -o -name '*.js' -o -name '*.json' -o -name '*.html' \
    -o -name '*.css' -o -name '*.dat' \) -size +1k -exec gzip -k -f -6 {} +
  find "$ctx/site/_framework" -name '*.gz' | while read -r gz; do rm -f "${gz%.gz}"; done

  # build.json: who this is, for the banner and the directory. CI sets PREVIEW_*; locally
  # git answers. A showcase.json in the site names the directory card's landing page.
  local showcase='{}'
  [[ -f "$site/showcase.json" ]] && showcase=$(jq -c . "$site/showcase.json")
  jq -n --arg branch "${PREVIEW_BRANCH:-$(git -C "$ROOT" branch --show-current)}" \
        --arg sha "${PREVIEW_SHA:-$(git -C "$ROOT" rev-parse HEAD)}" \
        --arg pr "${PREVIEW_PR:-}" --arg title "${PREVIEW_TITLE:-}" --arg author "${PREVIEW_AUTHOR:-}" \
        --arg built "$(date -u +%Y-%m-%dT%H:%MZ)" --arg path "$path" --argjson sc "$showcase" \
        '{$branch, $sha, $pr, $title, $author, $built, $path,
          showcase: ($sc.title // ""), landing: ($sc.landing // "cli.html")}' > "$ctx/site/build.json"
  echo "uploading $(du -sh "$ctx/site" | cut -f1)"

  # Extract beside the live directory, then rename in, so a half-upload is never served.
  # The staging name is this upload's own, so two pushes to one PR cannot share it.
  local dest="/data/$path" new="/data/$path.new-$RANDOM$RANDOM"
  tar -C "$ctx" -czf - site | on_host "set -e; mkdir -p '$new';
    tar -xzf - -C '$new' --strip-components=1; rm -rf '$dest'; mv '$new' '$dest'; regen-index"
  echo "https://$HOST/$path/"
}

remove() {
  valid_path "${1:-}"
  on_host "rm -rf '/data/$1' && regen-index" </dev/null
  echo "removed /$1/"
}

# Everything on the volume that is not latest/ or an open PR's pulls/<n>/ goes: closed PRs,
# stranded staging directories, and anything an older layout left behind.
cleanup() {
  local dry=false; [[ "${1:-}" == "--dry-run" ]] && dry=true
  local auth=(); [[ -n "${GITHUB_TOKEN:-}" ]] && auth=( -H "Authorization: Bearer $GITHUB_TOKEN" )
  local open="" page batch deployed gone=()
  for page in 1 2 3 4 5; do
    batch=$(curl -sS --fail "${auth[@]}" \
      "https://api.github.com/repos/$REPO/pulls?state=open&per_page=100&page=$page" | jq -r '.[].number')
    open+="$batch"$'\n'; [[ $(wc -l <<<"$batch") -eq 100 ]] || break
  done
  deployed=$(on_host "cd /data && ls -d * pulls/* 2>/dev/null; true" </dev/null | tr -d '\r')
  for d in $deployed; do
    case "$d" in
      latest|pulls|index.html|index.html.tmp|lost+found) ;;
      pulls/*) grep -qx "${d#pulls/}" <<<"$open" || gone+=("$d") ;;
      *) gone+=("$d") ;;
    esac
  done
  [ ${#gone[@]} -gt 0 ] || { echo "nothing to remove"; return; }
  printf 'removing /%s\n' "${gone[@]}"
  $dry || on_host "cd /data && rm -rf ${gone[*]} && regen-index" </dev/null
}

case "${1:-}" in
  upload|remove|cleanup) "$@" ;;
  *) sed -n '2,/^set /p' "$0" | sed '$d;s/^# \{0,1\}//'; exit 2 ;;
esac
