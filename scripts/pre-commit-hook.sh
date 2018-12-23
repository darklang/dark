#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "$@"

set -euo pipefail

# To install:
# - cp scripts/pre-commit.hook.sh .git/hooks/pre-commit
# - chmod +x .git/hooks/pre-commit

files=$(git diff --cached --name-only --diff-filter=ACM "*.ml" | tr '\n' ' ')
[ -z "$files" ] && exit 0

# Prettify all staged .js files
echo "$files" | xargs scripts/ocamlformat --inplace

# Add back the modified/prettified files to staging
echo "$files" | xargs git add

exit 0
