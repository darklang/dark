#!/bin/sh

# To install:
# - cp scripts/pre-commit.hook.sh .git/hooks/pre-commit
# - chmod +x .git/hooks/pre-commit

files=$(git diff --cached --name-only --diff-filter=ACM "*.ml" | tr '\n' ' ')
[ -z "$files" ] && exit 0

# Prettify all staged .js files
echo "$files" | xargs scripts/ocamlformat

# Add back the modified/prettified files to staging
echo "$files" | xargs git add

exit 0
