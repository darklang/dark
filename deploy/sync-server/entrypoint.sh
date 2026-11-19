#!/bin/sh
# Starts `dark sync serve` as the `dark` user, on the store in /data.
#
# The server's own settings, from the environment (the CLI itself reads none of these):
#   DARK_SYNC_LISTEN               where to listen               (default :8080)
#   DARK_SYNC_BACKUP_EVERY_HOURS   how often to back up the store (default 6)
#   DARK_SYNC_BACKUP_KEEP          how many backups to keep       (default 28: a week)
#   DARK_SYNC_MAX_FRAME_MB         the largest frame taken        (default 256)
#   DARK_SYNC_BOOTSTRAP_PERSON     with DARK_SYNC_BOOTSTRAP_TOKEN, a token kept on every start,
#   DARK_SYNC_BOOTSTRAP_TOKEN      so the first person can connect before anyone has a shell
#   DARK_SYNC_MAINTENANCE          when set, start without serving, so the store can be restored
set -eu

# A fresh volume is root's.
if [ "$(id -u)" = 0 ]; then
  chown dark:dark /data
  exec setpriv --reuid=dark --regid=dark --init-groups "$0" "$@"
fi

mkdir -p /data/backups

if [ -n "${DARK_SYNC_BOOTSTRAP_PERSON:-}" ] && [ -n "${DARK_SYNC_BOOTSTRAP_TOKEN:-}" ]; then
  printf '%s\n' "$DARK_SYNC_BOOTSTRAP_TOKEN" \
    | dark sync token add "$DARK_SYNC_BOOTSTRAP_PERSON" --label bootstrap
fi

if [ "$#" -gt 0 ]; then
  exec dark "$@"
fi

if [ -n "${DARK_SYNC_MAINTENANCE:-}" ]; then
  echo '{"event":"maintenance","note":"not serving; unset DARK_SYNC_MAINTENANCE to serve"}'
  exec sleep infinity
fi

exec dark sync serve \
  --listen "${DARK_SYNC_LISTEN:-:8080}" \
  --backup-dir /data/backups \
  --backup-every "${DARK_SYNC_BACKUP_EVERY_HOURS:-6}" \
  --keep "${DARK_SYNC_BACKUP_KEEP:-28}" \
  --max-frame-mb "${DARK_SYNC_MAX_FRAME_MB:-256}"
