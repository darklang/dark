#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "$@"

set -euo pipefail

# To install:
# - cp scripts/pre-commit-hook.sh .git/hooks/pre-commit
# - chmod +x .git/hooks/pre-commit

ocamlfiles=$(git diff --cached --name-only --diff-filter=ACM "*.ml" | tr '\n' ' ')

if [[ "$ocamlfiles" ]]; then
  # format all staged files
  echo "$ocamlfiles" | xargs -0L1 scripts/ocamlformat --inplace
  # Add back the modified/formatted files to staging
  echo "$ocamlfiles" | xargs -0L1 git add
fi

prettierfiles=$(git diff --cached --name-only --diff-filter=ACM "*.js" "*.html" | tr '\n' ' ')

if [[ "$prettierfiles" ]]; then
  # format all staged files
  echo "$prettierfiles" | xargs -0L1 client/node_modules/.bin/prettier --write
  # Add back the modified/formatted files to staging
  echo "$prettierfiles" | xargs -0L1 git add
fi

exit 0
