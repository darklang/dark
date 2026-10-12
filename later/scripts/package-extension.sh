#!/usr/bin/env bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"
set -euo pipefail


# Script to package VS Code extension

EXTENSION_DIR="./vscode-extension"
VSIX_DIR="./.vsix"

# Create VSIX directory if it doesn't exist
mkdir -p "$VSIX_DIR"

# Package the extension
echo "Packaging extension..."
cd "$EXTENSION_DIR"
npm i
vsce package --skip-license --pre-release

# Move the VSIX file to .vsix directory
echo "Moving VSIX file to $VSIX_DIR..."
mv -- *.vsix "../$VSIX_DIR/"

# Get the name of the VSIX file
cd ..
VSIX_FILE=$(find "$VSIX_DIR" -name "*.vsix" -type f -print0 | xargs -0 ls -t | head -1)
VSIX_FILENAME=$(basename "$VSIX_FILE")

echo "✅ Extension packaged successfully: $VSIX_FILENAME"
echo
echo "To use the darklang extension in the devcontainer, run:"
echo "code --install-extension $VSIX_DIR/$VSIX_FILENAME"