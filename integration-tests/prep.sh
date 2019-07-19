#!/usr/bin/env bash
. ./scripts/support/assert-in-container "$0" "$@"

set -euo pipefail

# Prep the integration tests. This needs to be run in the container

# kill the tmux sessions used to control ffmpeg
set +e
sessions=$(tmux list-sessions -F '#{session_name}' 2>/dev/null | grep integrationTests)
set -e
for s in ${sessions}; do
  tmux kill-session -t "$s"
done

# Set up test reporters for CircleCI
TEST_RESULTS_DIR="${DARK_CONFIG_RUNDIR}/test_results"
mkdir -p "${TEST_RESULTS_DIR}"

echo "Clearing old test files"
rm -f "${DARK_CONFIG_RUNDIR}"/completed_tests/*
rm -Rf "${DARK_CONFIG_RUNDIR}"/screenshots/*
rm -f "${DARK_CONFIG_RUNDIR}"/videos/*
rm -Rf "${DARK_CONFIG_RUNDIR}"/logs/ffmpeg*
rm -f "${TEST_RESULTS_DIR}"/integration_tests.*

# Clear DBs
DBLOG="${DARK_CONFIG_RUNDIR}/integration_db.log"
echo "Clearing old DB data (logs in ${DBLOG})"
DB="${DARK_CONFIG_DB_DBNAME}"
function run_sql { psql -d "$DB" -c "$@" >> "$DBLOG" 2>&1; }

function fetch_sql { psql -d "$DB" -t -c "$@"; }

CANVASES=$(fetch_sql "SELECT id FROM canvases WHERE substring(name, 0, 6)
= 'test-';")
SCRIPT=""
for cid in $CANVASES; do
  SCRIPT+="DELETE FROM events WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_results_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM stored_events_v2 WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM user_data WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM cron_records WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM toplevel_oplists WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM function_arguments_canvas_id_fkey WHERE canvas_id = '$cid';";
  SCRIPT+="DELETE FROM canvases WHERE id = '$cid';";
done
run_sql "$SCRIPT";
