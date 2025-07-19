#!/usr/bin/env bash

set -euo pipefail

DB_PATH="./rundir/data.db"
RETRY_INTERVAL=5

echo "SQLite3 Monitor - Connecting to $DB_PATH"
echo "This terminal will automatically reconnect if the database file disappears and reappears."
echo "Use .quit to exit sqlite3, or Ctrl+C to stop monitoring."
echo ""

# Function to check if database file exists and is accessible
check_db() {
    if [[ -f "$DB_PATH" ]]; then
        # Try to open the database to ensure it's not corrupted
        if sqlite3 "$DB_PATH" ".schema" >/dev/null 2>&1; then
            return 0
        else
            echo "Database file exists but appears corrupted, waiting for recovery..."
            return 1
        fi
    else
        return 1
    fi
}

# Function to start sqlite3 session
start_sqlite() {
    echo "Connecting to $DB_PATH..."
    echo "SQLite version: $(sqlite3 --version)"
    echo "Database file size: $(du -h "$DB_PATH" 2>/dev/null | cut -f1 || echo 'unknown')"
    echo ""
    echo "Common commands:"
    echo "  .tables          - List all tables"
    echo "  .schema          - Show database schema"
    echo "  .quit            - Exit sqlite3"
    echo ""
    echo "NOTE: Running in read-only mode to avoid blocking write operations."
    echo ""

    # Start sqlite3 with some useful settings and interactive mode in read-only mode
    # This prevents the monitor from blocking write operations by other processes
    sqlite3 "file:$DB_PATH?mode=ro" -cmd ".headers on" -cmd ".mode column" -cmd ".width 20 20 20"
}

# Trap to handle cleanup
cleanup() {
    echo ""
    echo "SQLite monitor stopped."
    exit 0
}

trap cleanup SIGINT SIGTERM

# Main monitoring loop
while true; do
    if check_db; then
        start_sqlite
        # If sqlite3 exits, check if user wants to continue monitoring
        echo ""
        echo "SQLite session ended. Checking if database is still available..."
        if ! check_db; then
            echo "Database no longer available. Monitoring for recovery..."
        else
            echo "Database still available. Restarting sqlite3 session in 2 seconds..."
            echo "Press Ctrl+C to stop monitoring."
            sleep 2
        fi
    else
        echo "Database file $DB_PATH not found or not accessible."
        echo "Waiting ${RETRY_INTERVAL}s before retrying..."
        echo "Press Ctrl+C to stop monitoring."
        sleep $RETRY_INTERVAL
    fi
done