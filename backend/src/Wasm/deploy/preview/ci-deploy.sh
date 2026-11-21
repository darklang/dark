#!/usr/bin/env bash
# CI: decide whether this build gets a preview, upload it, tell the PR.
#
#   preview/ci-deploy.sh [site-dir]     upload; comment on the PR; on a release, clean up
#   preview/ci-deploy.sh --decide       print the path this build would go to, or exit 3
#                                       (build-wasm halts on that before the publish)
#
# Two things are hosted. latest/ is the newest release tag, from darklang/dark's own tag
# pipeline. pulls/<n>/ is an open PR on darklang/dark whose head is the branch this
# pipeline built. Nothing here decides who is trusted: a pipeline deploys only if its
# CircleCI project holds PREVIEW_FLY_API_TOKEN, and CircleCI never hands project secrets
# to a forked PR's build. See the README.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="${PREVIEW_REPO:-darklang/dark}"
DECIDE=false; [[ "${1:-}" == "--decide" ]] && { DECIDE=true; shift; }
skip() { echo "$1; not deploying a preview"; $DECIDE && exit 3 || exit 0; }

[[ -n "${PREVIEW_FLY_API_TOKEN:-}" ]] \
  || skip "PREVIEW_FLY_API_TOKEN is not in this project's environment (a forked PR's build never has it)"
[[ -z "${CIRCLE_PR_NUMBER:-}" ]] || skip "this is a forked PR's build"
export FLY_API_TOKEN="$PREVIEW_FLY_API_TOKEN" GITHUB_TOKEN="${PREVIEW_GITHUB_TOKEN:-}"
gh() { curl -sS --fail ${GITHUB_TOKEN:+-H "Authorization: Bearer $GITHUB_TOKEN"} "$@"; }

sha="${CIRCLE_SHA1:-$(git rev-parse HEAD)}"
pr=""; title=""; author=""
if [[ -n "${CIRCLE_TAG:-}" ]]; then
  # A release: the tag itself, strictly, from darklang's CI, and only the newest one, so a
  # pipeline that finishes late never puts an older release over a newer one.
  [[ "$CIRCLE_TAG" =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]] || skip "tag $CIRCLE_TAG is not a release tag"
  [[ "${CIRCLE_PROJECT_USERNAME:-}" == "darklang" ]] || skip "releases deploy from darklang's CI only"
  newest=$(git ls-remote --tags --refs "https://github.com/$REPO.git" 'v*' \
    | sed 's|.*refs/tags/||' | grep -E '^v[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -n 1)
  [[ "$CIRCLE_TAG" == "$newest" ]] || skip "$CIRCLE_TAG is not the newest release ($newest is)"
  branch="$CIRCLE_TAG"; path="latest"
else
  # A branch: the open PR whose head it is, asked of GitHub rather than CircleCI, which
  # only knows about a PR from the push after it was opened.
  branch="${CIRCLE_BRANCH:-$(git branch --show-current)}"
  owner="${CIRCLE_PROJECT_USERNAME:-${REPO%%/*}}"
  head=$(jq -rn --arg h "$owner:$branch" '$h | @uri')
  meta=$(gh "https://api.github.com/repos/$REPO/pulls?state=open&head=$head" | jq '.[0] // empty')
  [[ -n "$meta" ]] || skip "no open PR on $REPO from $owner:$branch"
  pr=$(jq -r .number <<<"$meta"); title=$(jq -r .title <<<"$meta"); author=$(jq -r .user.login <<<"$meta")
  path="pulls/$pr"
fi
$DECIDE && { echo "$path"; exit 0; }

url=$(PREVIEW_BRANCH="$branch" PREVIEW_SHA="$sha" PREVIEW_PR="$pr" PREVIEW_TITLE="$title" PREVIEW_AUTHOR="$author" \
      "$HERE/host.sh" upload "$path" "$@" | tee /dev/stderr | tail -n 1)
[[ "$path" != "latest" ]] || "$HERE/host.sh" cleanup || echo "cleanup failed; the next release retries"
[[ -n "$pr" && -n "$GITHUB_TOKEN" ]] || exit 0

# One comment per PR: find ours by the marker and edit it, else create it.
marker="<!-- dark-preview -->"
body="$marker
Browser CLI built from this branch, at ${sha:0:7}: $url

\`cli.html?cmd=<argv>\` runs a command first, \`?fn=<name>\` lands on an item, \`eval.html?e=<expr>\` is an expression box. Nothing persists between reloads."
existing=$(gh "https://api.github.com/repos/$REPO/issues/$pr/comments?per_page=100" \
  | jq -r --arg m "$marker" 'map(select(.body | startswith($m)))[0].id // empty')
payload=$(jq -n --arg body "$body" '{body:$body}')
if [[ -n "$existing" ]]; then gh -X PATCH "https://api.github.com/repos/$REPO/issues/comments/$existing" -d "$payload" >/dev/null
else gh -X POST "https://api.github.com/repos/$REPO/issues/$pr/comments" -d "$payload" >/dev/null; fi
echo "PR comment ${existing:+updated}${existing:-created}"
