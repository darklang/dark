#!/bin/bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# this fetches and builds the tree-sitter library,
# to call upon within our Darklang bindings,
# in conjunction with the tree-sitter-darklang parser
# which is also in this repo.
#
# The tree-sitter library is built for multiple platforms,
# as we build the CLI executable for multiple OS+arch combinations.
# The resultant libraries are copied into the backend/src/LibTreeSitter/lib directory,
# packaged along with our CLI executables, and used by the Darklang bindings.
#
# Note: The list of build targets should be updated _in conjunction with_ equivalent
# updates in `LibTreeSitter.fsproj`, as well as `build-release-cli-exes.sh`.

cd ~/ || exit

# Clone the specific branch of the tree-sitter repository
git clone --depth 1 --branch v0.20.8 https://github.com/tree-sitter/tree-sitter.git

# Base output directory for compiled libraries
output_base_dir="app/backend/src/LibTreeSitter/lib"
tree_sitter_sources="tree-sitter/lib/src/lib.c -I tree-sitter/lib/src -I tree-sitter/lib/src/../include"

mkdir -p $output_base_dir

# Compile tree-sitter for different platforms using the function
parallel ::: \
  "$HOME/zig/zig cc -fPIC -shared -o $output_base_dir/tree-sitter.so $tree_sitter_sources" \
  "$HOME/zig/zig cc -target x86_64-linux-gnu -fPIC -shared -o $output_base_dir/tree-sitter-linux-x64.so $tree_sitter_sources" \
  "$HOME/zig/zig cc -target x86_64-linux-musl -fPIC -shared -o $output_base_dir/tree-sitter-linux-musl-x64.so $tree_sitter_sources" \
  "$HOME/zig/zig cc -target aarch64-linux-gnu -fPIC -shared -o $output_base_dir/tree-sitter-linux-arm64.so $tree_sitter_sources" \
  "$HOME/zig/zig cc -target arm-linux-gnueabihf -fPIC -shared -o $output_base_dir/tree-sitter-linux-arm.so $tree_sitter_sources" \
  "$HOME/zig/zig cc -target x86_64-macos-none -fPIC -shared -o $output_base_dir/tree-sitter-macos-x64.dylib $tree_sitter_sources" \
  "$HOME/zig/zig cc -target aarch64-macos-none -fPIC -shared -o $output_base_dir/tree-sitter-macos-arm64.dylib $tree_sitter_sources"
  # "$HOME/zig/zig cc -target wasm32-freestanding -fPIC -shared -o $output_base_dir/tree-sitter.wasm $tree_sitter_sources"

# Clean up
rm -rf tree-sitter/