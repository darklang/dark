#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

# Clear DBs

DBLOG="${DARK_CONFIG_RUNDIR}/clear-db.log"
echo "Clearing old DB data (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"

function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" ; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE substring(name, 0, 6)
= 'test-';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM scheduling_rules_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM traces_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM trace_old_function_arguments_v0 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases_v0 WHERE id = '$cid';";
  SCRIPT+="DELETE FROM secrets_v0 WHERE canvas_id = '$cid';";
done

SCRIPT+="DELETE FROM packages_v0 WHERE author_id IN (SELECT id FROM accounts_v0
WHERE username = 'test_admin');";

run_sql "$SCRIPT";