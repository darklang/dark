# Global-variable recursion, so no subshell per call: the fastest honest bash version.
fib() {
  if (($1 < 2)); then
    r=$1
    return
  fi
  local n=$1 a b
  fib $((n - 1)); a=$r
  fib $((n - 2)); b=$r
  r=$((a + b))
}
fib 24
echo "result=$r"
