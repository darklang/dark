#!/usr/bin/env bash
. ./scripts/devcontainer/assert-in-container "$0" "$@"

set -euo pipefail

# Prep the integration tests. This needs to be run in the container

# kill the tmux sessions used to control ffmpeg
# set +e
# sessions=$(tmux list-sessions -F '#{session_name}' 2>/dev/null | grep integrationTests)
# set -e
# for s in ${sessions}; do
#   tmux kill-session -t "$s"
# done
#
# Set up test reporters for CircleCI
TEST_RESULTS_DIR="${DARK_CONFIG_RUNDIR}/test_results"
TEST_LOGS_DIR="${DARK_CONFIG_RUNDIR}/integration_test_logs"
mkdir -p "${TEST_RESULTS_DIR}"
mkdir -p "${TEST_LOGS_DIR}"

echo "Clearing old test files"
rm -f "${DARK_CONFIG_RUNDIR}"/completed_tests/*
rm -Rf "${DARK_CONFIG_RUNDIR}"/screenshots/*
rm -Rf "${DARK_CONFIG_RUNDIR}"/videos/*
rm -Rf "${DARK_CONFIG_RUNDIR}"/logs/ffmpeg*
rm -f "${TEST_RESULTS_DIR}"/integration_tests.*
rm -f "${TEST_LOGS_DIR}"/*

./integration-tests/clear-db.sh
