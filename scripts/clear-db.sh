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

SCRIPT+="DELETE FROM packages_v0 WHERE author_id IN (SELECT id FROM accounts
WHERE username = 'test_admin');";

# This is not really a 'clear-db' action, but we want to seed a package_v0 so
# that we can check (in integration tests) that it's in autocomplete, can run
# in-browser and in-bwd, etc
SCRIPT+="INSERT INTO packages_v0 (tlid, user_id, package, module, fnname,
version, description, body, body2, return_type, parameters, author_id, deprecated,
updated_at, created_at) VALUES
( '4186046771064433369', (SELECT id FROM accounts WHERE username = 'test_admin'), 'stdlib',
'Test', 'one', 1, '', ''::bytea, decode('kgCSzjq57RkA', 'base64')::bytea, 'Any',
'[]'::jsonb,
(SELECT id FROM accounts WHERE username = 'test_admin'), False, now(), now());";
run_sql "$SCRIPT";