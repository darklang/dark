# The same work without leaving bash: one `read` per line and bash's own substring test. Kept as a
# second row so the grep number above is not mistaken for bash's text-scanning speed.
a=0
b=0
while IFS= read -r l; do
  [[ $l == *"LEVEL="* ]] && ((a++))
  [[ $l == *ERROR* ]] && ((b++))
done < rundir/crosslang-lines.txt
echo "result=$((a + b))"
