work() { local n=$1 i; local -a out=(); for ((i=1;i<=n;i++)); do out+=($((i+1))); done; WR=${#out[@]}; }
repeat() { local times=$1 n=$2 acc=$3 i; for ((i=0;i<times;i++)); do work "$n"; acc=$((acc+WR)); done; RES=$acc; }
repeat 20 50 0
s=$(date +%s%N); repeat 200 50 0; e=$(date +%s%N)
echo "bash    elapsed_ms=$(( (e-s)/1000000 ))  result=$RES"
