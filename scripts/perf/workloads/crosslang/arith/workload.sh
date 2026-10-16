acc=0
for ((i = 1; i <= 300000; i++)); do
  ((acc = (acc + i * i) % 1000003))
done
echo "result=$acc"
