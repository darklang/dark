#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

set -euo pipefail

# Clear DBs

DBLOG="${DARK_CONFIG_RUNDIR}/integration-tests/clear-db.log"
echo "Clearing old DB data (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"

function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" ; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

# If you're looking to purge a _lot_ of data, here's some advice:
#
# - uncomment these lines and run the script to see how many delete-able canvases you have:
#CANVAS_COUNT=$(fetch_sql "SELECT count(1) FROM canvases WHERE substring(name, 0, 6) = 'test-'")
#echo "canvases $CANVAS_COUNT"
# - edit the CANVASES below to include a `LIMIT 143`,
#   (which is apparently the most the script can handle at once.)
# - figure out how many times you need to run the script (CANVAS_COUNT / 143)
# - run the script that # of times with
#   `for i in `seq 1 [CANVAS_COUNT / 143]`; do ./integration-tests/clear-db.sh; done`
#
# Ideally, we'd replace this hacky system with a better script,
# but this is something for now.

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE substring(name, 0, 6)
= 'test-';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM scheduling_rules WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM events WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v3 WHERE canvas_id = '$cid';";
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
version, description, body, return_type, parameters, author_id, deprecated,
updated_at, created_at) VALUES
( '4186046771064433369', (SELECT id FROM accounts WHERE username = 'test_admin'), 'stdlib',
'Test', 'one', 1, '', decode('/NlqdxJcXNMXOgH9v2pBKQYBMAo=', 'base64')::bytea, 'Any',
'[]'::jsonb,
(SELECT id FROM accounts WHERE username = 'test_admin'), False, now(), now());";
run_sql "$SCRIPT";
