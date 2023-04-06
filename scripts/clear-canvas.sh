#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

DBLOG="${DARK_CONFIG_RUNDIR}/logs/clear-canvas.log"
echo "Clearing canvas (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"

function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" ; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT canvas_id FROM domains_v0 where domain ='$1.dlio.localhost';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM scheduling_rules_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM traces_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM trace_old_events_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases_v0 WHERE id = '$cid';";
  SCRIPT+="DELETE FROM domains_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM secrets_v0 WHERE canvas_id = '$cid';";
done

run_sql "$SCRIPT";