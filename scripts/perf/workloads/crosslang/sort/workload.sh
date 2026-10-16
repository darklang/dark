# bash has no sort: `sort -n` is a separate C program, and piping to it is what a shell programmer
# would really write. The row therefore measures coreutils plus bash's loop, not bash sorting.
s=1
out=$(for ((i = 0; i < 50000; i++)); do
  ((s = (s * 48271) % 2147483647))
  echo "$s"
done | sort -n)
lo=$(head -1 <<< "$out")
hi=$(tail -1 <<< "$out")
n=$(wc -l <<< "$out")
echo "result=$((lo + hi + n))"
