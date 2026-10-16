acc=0
for ((it = 0; it < 1000; it++)); do
  declare -A d=()
  for ((i = 1; i <= 50; i++)); do d["k$i"]=$i; done
  for ((i = 1; i <= 50; i++)); do ((acc += d["k$i"])); done
  unset d
done
echo "result=$acc"
