#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

DBLOG="${DARK_CONFIG_RUNDIR}/logs/clear-dark-editor-canvas.log"
echo "Clearing dark-editor canvas (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"

function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" ; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT id FROM canvases_v0 WHERE name= 'dark-editor';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM scheduling_rules_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM traces_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_arguments_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases_v0 WHERE id = '$cid';";
  SCRIPT+="DELETE FROM secrets_v0 WHERE canvas_id = '$cid';";
done

run_sql "$SCRIPT";