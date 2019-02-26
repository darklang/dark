#!/bin/bash

set -euo pipefail

RELEASE=""
for i in "$@"; do
    case "${i}" in
        --release)
        RELEASE="${i}"
        shift
        ;;
    esac
done


set -x
cargo build --target x86_64-unknown-linux-gnu ${RELEASE}
PATH=../../target/bin:$PATH cargo build --target x86_64-apple-darwin ${RELEASE}
cargo build --target x86_64-pc-windows-gnu ${RELEASE}
cargo build --target x86_64-unknown-linux-musl --features vendored ${RELEASE}
