#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "$@"

set -euo pipefail

# Clear DBs

DBLOG="${DARK_CONFIG_RUNDIR}/integration_db.log"
echo "Clearing old DB data (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"

function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" ; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE substring(name, 0, 6)
= 'test-';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM scheduling_rules WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM events WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_arguments WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM static_asset_deploys WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases WHERE id = '$cid';";
done
run_sql "$SCRIPT";
