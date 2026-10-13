acc=0
for ((it = 0; it < 2000; it++)); do
  xs=()
  for ((i = 1; i <= 50; i++)); do xs+=("$i"); done
  ys=()
  for x in "${xs[@]}"; do ys+=($((x + 1))); done
  zs=()
  for y in "${ys[@]}"; do ((y % 2 == 0)) && zs+=("$y"); done
  ((acc += ${#zs[@]}))
done
echo "result=$acc"
