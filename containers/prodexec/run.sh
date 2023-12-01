#!/usr/bin/env bash

set -euo pipefail

# Set up chisel
chisel --version
cat <<EOF > /home/dark/auth.json
{
  "$DARK_CONFIG_PRODEXEC_CHISEL_USERNAME:$DARK_CONFIG_PRODEXEC_CHISEL_PASSWORD": ["localhost:22"]
}
EOF
nohup chisel server --authfile /home/dark/auth.json --port "$DARK_CONFIG_PRODEXEC_PORT" -v &


# Set up ssh auth
echo "dark:$DARK_CONFIG_PRODEXEC_SSH_PASSWORD" | sudo chpasswd
sudo service ssh start


# Add config vars to env for users logging in
env | grep DARK_CONFIG | sed 's/\(.*\)/export \1/g' >> ~/.bashrc


while true; do
  sleep 1
done
