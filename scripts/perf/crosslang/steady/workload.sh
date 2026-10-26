acc=0
for ((it = 0; it < 200; it++)); do
  ys=()
  for ((i = 1; i <= 50; i++)); do ys+=($((i + 1))); done
  ((acc += ${#ys[@]}))
done
echo "result=$acc"
