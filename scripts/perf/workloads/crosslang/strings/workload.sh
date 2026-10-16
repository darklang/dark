acc=0
for ((i = 1; i <= 5000; i++)); do
  base="field-$i"
  parts=()
  for ((j = 1; j <= 8; j++)); do parts+=("$base:$j"); done
  joined=$(IFS=,; echo "${parts[*]}")
  IFS=, read -ra back <<< "$joined"
  ((acc += ${#joined} + ${#back[@]}))
done
echo "result=$acc"
