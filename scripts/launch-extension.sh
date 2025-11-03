#!/bin/bash

# Launch VS Code extension in pure host environment
# Run this from the HOST machine, not from inside devcontainer

# Detect workspace root from script location (script is in workspace/scripts/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(dirname "$SCRIPT_DIR")"
EXT_PATH="$WORKSPACE_ROOT/vscode-extension"
USER_DATA_DIR="/tmp/vscode-extension-dev-$(date +%s)"

echo "Launching extension development host..."
echo "Extension path: $EXT_PATH"
echo "User data dir: $USER_DATA_DIR"

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
  "workbench.activity.pinnedViewlets2": "[\\"workbench.view.extension.darklang\\",\\"workbench.view.explorer\\",\\"workbench.view.search\\",\\"workbench.view.scm\\",\\"workbench.view.debug\\",\\"workbench.view.extensions\\"]",
  "workbench.sidebar.activeViewletId": "workbench.view.extension.darklang"
}
EOF

# Launch VS Code with extension
# Return to workspace root and open it as the workspace
cd "$WORKSPACE_ROOT" || exit
VSCODE_DEBUG_MODE=true code \
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