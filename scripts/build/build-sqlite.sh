#!/bin/bash
. ./scripts/devcontainer/_assert-in-container "$0" "$@"

# this fetches and builds the Sqlite DB engine,
# to call upon within our Darklang bindings.
#
# We build Sqlite built for multiple platforms,
# as we build the CLI executable for multiple OS+arch combinations.
# The resultant libraries are copied into the backend/src/Cli directory,
# packaged along with our CLI executables, and used by the Darklang bindings.
#
# Note: The list of build targets should be updated _in conjunction with_ equivalent
# updates in `LibTreeSitter.fsproj`, as well as `build-release-cli-exes.sh`.

cd ~/ || exit

# Download the specific version of the SQLite amalgamation
sqlite_url="https://www.sqlite.org/2024/sqlite-amalgamation-3460000.zip"
#sqlite_sha3="1221eed70de626871912bfca144c00411f0c30d3c2b7935cff3963b63370ef7c"

# Download (TODO and verify) SQLite amalgamation
curl -O $sqlite_url
echo "$sqlite_sha3  sqlite-amalgamation-3460000.zip" #| sha3sum -c -

# Unzip the SQLite amalgamation
unzip sqlite-amalgamation-3460000.zip

# Base output directory for compiled libraries
output_base_dir="app/backend/src/Cli/lib"
sqlite_sources="sqlite-amalgamation-3460000/sqlite3.c -I sqlite-amalgamation-3460000"

mkdir -p $output_base_dir
sqlite_compile_flags="-Os -DSQLITE_OMIT_SHARED_CACHE -DSQLITE_OMIT_LOAD_EXTENSION -DSQLITE_OMIT_EXPLAIN -DSQLITE_OMIT_DEPRECATED"

# Compile SQLite for different platforms using the function
parallel ::: \
  "$HOME/zig/zig cc $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3.so $sqlite_sources" \
  "$HOME/zig/zig cc -target x86_64-linux-gnu $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3-linux-x64.so $sqlite_sources" \
  "$HOME/zig/zig cc -target x86_64-linux-musl $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3-linux-musl-x64.so $sqlite_sources" \
  "$HOME/zig/zig cc -target aarch64-linux-gnu $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3-linux-arm64.so $sqlite_sources" \
  "$HOME/zig/zig cc -target arm-linux-gnueabihf $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3-linux-arm.so $sqlite_sources" \
  "$HOME/zig/zig cc -target x86_64-macos-none $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3-macos-x64.dylib $sqlite_sources" \
  "$HOME/zig/zig cc -target aarch64-macos-none $sqlite_compile_flags -fPIC -shared -o $output_base_dir/sqlite3-macos-arm64.dylib $sqlite_sources"

# Clean up
rm -rf sqlite-amalgamation-3460000*