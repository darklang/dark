#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

# To install:
# - cp scripts/formatting/pre-commit-hook.sh .git/hooks/pre-commit
# - chmod +x .git/hooks/pre-commit

# Works on all filetype, silently ignoring unsupported files

files=$(git diff --cached --name-only --diff-filter=ACM)

# format all staged files
echo "$files" | xargs scripts/formatting/format check --quiet
# Add back the modified/formatted files to staging
echo "$files" | xargs git add

exit 0
