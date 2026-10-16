# NOT part of the comparison: bash segfaults at depth 5000 (it manages 4000). Kept so the appendix
# shows what was tried.
#
# It returns through a global rather than through command substitution. The command-substitution
# version is the idiomatic one and it is unusable here: each level forks a subshell that stays
# alive until the recursion unwinds, so depth 5000 means 5000 live processes. I ran it once and it
# put 1,469 of them on the box before I killed it.
depth() {
  if (($1 <= 0)); then
    r=0
    return
  fi
  local n=$1
  depth $((n - 1))
  r=$((1 + r))
}
acc=0
for ((it = 0; it < 50; it++)); do
  depth 5000
  ((acc += r))
done
echo "result=$acc"
