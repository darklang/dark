#!/bin/bash

# Launch VS Code extension for development/testing
#
# Usage:
#   ./scripts/launch-extension.sh            # Prompts for mode
#   ./scripts/launch-extension.sh --release  # Uses installed 'dark' CLI (run from host)
#   ./scripts/launch-extension.sh --debug    # Uses ./scripts/run-cli (run from container)

# Parse arguments
MODE=""
for arg in "$@"; do
  case $arg in
    --debug)
      MODE="debug"
      ;;
    --release)
      MODE="release"
      ;;
  esac
done

# If no mode specified, ask
if [[ -z "$MODE" ]]; then
  echo "Select extension mode:"
  echo "  1) release - uses installed 'dark' CLI (for host testing)"
  echo "  2) debug   - uses ./scripts/run-cli (for container testing)"
  read -rp "Enter choice [1/2]: " choice
  case $choice in
    1) MODE="release" ;;
    2) MODE="debug" ;;
    *)
      echo "Invalid choice. Use --release or --debug flag."
      exit 1
      ;;
  esac
fi

# Detect workspace root from script location (script is in workspace/scripts/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(dirname "$SCRIPT_DIR")"
EXT_PATH="$WORKSPACE_ROOT/vscode-extension"
USER_DATA_DIR="/tmp/vscode-extension-dev-$(date +%s)"

echo ""
echo "Launching extension development host..."
echo "Extension path: $EXT_PATH"
echo "User data dir: $USER_DATA_DIR"
if [[ "$MODE" == "debug" ]]; then
  echo "Mode: debug (using ./scripts/run-cli)"
else
  echo "Mode: release (using 'dark' CLI)"
fi

# Kill any existing code processes that might interfere
pkill -f "extensionDevelopmentPath" || true

# Initial build - fail fast if compilation fails
echo "Building extension..."
cd "$EXT_PATH" || exit
if ! npm run compile; then
  echo "ERROR: Initial compilation failed. Fix errors and try again."
  exit 1
fi
echo "Initial build successful!"

# Start watch mode in background (will recompile on changes)
echo "Starting watch mode for auto-recompilation..."
npm run watch > /tmp/vscode-extension-watch.log 2>&1 &
WATCH_PID=$!
echo "Watch process started (PID: $WATCH_PID)"
echo "Watch logs: /tmp/vscode-extension-watch.log"

# Give watch a moment to start
sleep 2

# Create settings with larger zoom level for better readability
mkdir -p "$USER_DATA_DIR/User"
cat > "$USER_DATA_DIR/User/settings.json" <<EOF
{
  "window.zoomLevel": 2,
  "editor.fontSize": 14,
  "terminal.integrated.fontSize": 14,
  "workbench.view.extension.darklang.visible": true,
  "workbench.activityBar.visible": true
}
EOF

# Create workspace state to open Darklang viewcontainer first
mkdir -p "$USER_DATA_DIR/User/workspaceStorage/default"
cat > "$USER_DATA_DIR/User/globalStorage/storage.json" <<EOF
{
  "workbench.activity.pinnedViewlets2": "[\"workbench.view.extension.darklang\",\"workbench.view.explorer\",\"workbench.view.search\",\"workbench.view.scm\",\"workbench.view.debug\",\"workbench.view.extensions\"]",
  "workbench.sidebar.activeViewletId": "workbench.view.extension.darklang"
}
EOF

# Launch VS Code with extension
cd "$WORKSPACE_ROOT" || exit

# Set DARKLANG_CLI_MODE if debug mode requested
if [[ "$MODE" == "debug" ]]; then
  export DARKLANG_CLI_MODE="debug"
fi

code \
  --extensionDevelopmentPath="$EXT_PATH" \
  --disable-extensions \
  --disable-extension github.copilot \
  --disable-extension github.copilot-chat \
  --user-data-dir="$USER_DATA_DIR" \
  --no-sandbox \
  --disable-workspace-trust \
  --verbose \
  "$WORKSPACE_ROOT"

echo ""
echo "Extension development host launched!"
echo ""
echo "IMPORTANT: To reload after code changes:"
echo "  1. Save your TypeScript files"
echo "  2. Wait for compilation (check /tmp/vscode-extension-watch.log)"
echo "  3. Press Ctrl+R (or Cmd+R on Mac) in the extension window to reload"
echo ""
echo "To stop watch mode: kill $WATCH_PID"
