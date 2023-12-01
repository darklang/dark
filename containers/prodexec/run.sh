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


# Add config vars to env for users logging in.  This loops through env vars, then
# adds export and saves them. This handles newlines correctly, needed for certs and
# other auth.
env_vars=$(compgen -v | grep ^DARK_CONFIG)
for var in $env_vars
do
  typeset -p $var | sed 's/^declare -x/export/' >> /home/dark/.bashrc
done


while true; do
  sleep 1
done
