#!/usr/bin/env bash
set -euo pipefail
./scripts/support/assert-in-container $0 $@

DATE=`date '+%Y_%m_%d-%H_%M_%S'`
DEST="${DARK_CONFIG_PERSIST_DIR}/backup_appdata/${DATE}"

mkdir -p $DEST

cp -R "${DARK_CONFIG_PERSIST_DIR}/appdata" "${DEST}"

echo "Copied into $DEST"
