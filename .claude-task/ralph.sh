#!/bin/bash
# Ralph Wiggum loop - runs Claude until task complete
set -e

TASK_DIR=".claude-task"
PHASE_FILE="$TASK_DIR/phase"
MAX_ITERATIONS=${MAX_ITERATIONS:-100}
ITERATION=0

mkdir -p "$TASK_DIR"
echo "executing" > "$PHASE_FILE"

log() {
    echo "[ralph] $1"
    echo "$(date '+%H:%M:%S') $1" >> "$TASK_DIR/loop.log"
}

log "Starting Ralph loop (max $MAX_ITERATIONS iterations)"

while [ $ITERATION -lt $MAX_ITERATIONS ]; do
    ITERATION=$((ITERATION + 1))

    phase=$(cat "$PHASE_FILE" 2>/dev/null || echo "executing")
    if [ "$phase" = "done" ]; then
        log "Task complete!"
        break
    fi

    log "Iteration $ITERATION - running Claude"

    # Run Claude with a prompt - it reads CLAUDE.md which has the task context
    claude --dangerously-skip-permissions -p "Continue working on the task. Read CLAUDE.md for context and .claude-task/todos.md for the checklist. Complete the next unchecked todo." || true

    # Check phase after Claude exits
    phase=$(cat "$PHASE_FILE" 2>/dev/null || echo "executing")
    if [ "$phase" = "done" ]; then
        log "Task complete!"
        break
    fi

    log "Claude exited, restarting in 2s..."
    sleep 2
done

if [ $ITERATION -ge $MAX_ITERATIONS ]; then
    log "Max iterations reached"
fi

log "Loop finished"
