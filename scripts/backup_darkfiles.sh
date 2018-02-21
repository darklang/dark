#!/usr/bin/env bash

set -euo pipefail

DATE=`date '+%Y_%m_%d-%H_%M_%S'`
DEST="server/backup_appdata/${DATE}"

cp -R server/appdata "${DEST}"

echo "Copied into $DEST"
