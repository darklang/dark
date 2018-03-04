#!/usr/bin/env bash

set -euo pipefail

DATE=`date '+%Y_%m_%d-%H_%M_%S'`
DEST="runtime/backup_appdata/${DATE}"

mkdir -p $DEST

cp -R runtime/appdata "${DEST}"

echo "Copied into $DEST"
