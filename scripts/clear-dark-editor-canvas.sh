#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

DBLOG="${DARK_CONFIG_RUNDIR}/logs/clear-dark-editor-canvas.log"
echo "Clearing dark-editor canvas (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"

function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" ; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE name= 'dark-editor';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM scheduling_rules WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM events WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v3 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM traces_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_arguments WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM static_asset_deploys WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases WHERE id = '$cid';";
  SCRIPT+="DELETE FROM secrets WHERE canvas_id = '$cid';";
done

run_sql "$SCRIPT";