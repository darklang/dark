#!/bin/sh
_term() {
  >&2 echo "TERM (entrypoint.sh)"
  exit 0
}
trap "_term" TERM

if [ "$CHISEL_SERVER" = "" ]; then
  echo "CHISEL_SERVER missing"
  sleep 1
  exit 1
fi
# dummy http server to satisfy cloud run PORT bind
>/dev/null 2>&1 chisel server &

sudo service run ssh

while true; do
  set +e
    chisel client "$CHISEL_SERVER" R:2222:localhost:2222
  set -e
  sleep 1
done
