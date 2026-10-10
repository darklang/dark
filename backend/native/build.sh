#!/usr/bin/env bash
# No libc dependency: build the same freestanding transport for JIT and AOT.
set -euo pipefail
out=$1
rid=$2
src=$(cd "$(dirname "$0")" && pwd)/syscalls.c
mkdir -p "$out"
case "$rid" in
  linux-x64|linux-musl-x64) target=x86_64-linux-gnu; extension=so ;;
  linux-arm64|linux-musl-arm64) target=aarch64-linux-gnu; extension=so ;;
  linux-arm) target=arm-linux-gnueabihf; extension=so ;;
  osx-x64) target=x86_64-macos; extension=dylib ;;
  osx-arm64) target=aarch64-macos; extension=dylib ;;
  *) echo "Unsupported syscall target: $rid" >&2; exit 1 ;;
esac
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) native=linux-x64 ;;
  Linux-aarch64) native=linux-arm64 ;;
  Linux-armv7l) native=linux-arm ;;
  Darwin-x86_64) native=osx-x64 ;;
  Darwin-arm64) native=osx-arm64 ;;
  *) native=unknown ;;
esac
if [[ "$rid" == "$native" || "$rid" == linux-musl-x64 && "$native" == linux-x64 ]]; then
  compiler=("${CC:-cc}")
  archiver=("${AR:-ar}")
else
  # The release toolchain already uses Zig for cross-platform native libraries.
  compiler=(zig cc -target "$target")
  archiver=(zig ar)
fi
"${compiler[@]}" -O2 -fPIC -ffreestanding -fno-builtin -fno-stack-protector -fomit-frame-pointer -c "$src" -o "$out/syscalls.o"
"${archiver[@]}" rcs "$out/libdark_syscalls.a" "$out/syscalls.o"
if [[ "$extension" == so ]]; then
  "${compiler[@]}" -shared -nostdlib -Wl,--no-undefined "$out/syscalls.o" -o "$out/libdark_syscalls.so"
else
  "${compiler[@]}" -dynamiclib -nostdlib -Wl,-install_name,@rpath/libdark_syscalls.dylib "$out/syscalls.o" -o "$out/libdark_syscalls.dylib"
fi
