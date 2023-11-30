#!/usr/bin/env bash

set -euo pipefail

_term() {
  >&2 echo "TERM (entrypoint.sh)"
  exit 0
}
trap "_term" TERM

chisel --version

cat <<EOF > /home/dark/auth.json
{
  "$DARK_CONFIG_PRODEXEC_CHISEL_USERNAME:$DARK_CONFIG_PRODEXEC_CHISEL_PASSWORD": ["localhost:22"]
}
EOF

nohup chisel server --authfile /home/dark/auth.json --port "$DARK_CONFIG_PRODEXEC_PORT" -v &

RUN echo "dark:$DARK_CONFIG_PRODEXEC_SSH_PASSWORD" | sudo chpasswd

sudo service ssh start

while true; do
  sleep 1
done
