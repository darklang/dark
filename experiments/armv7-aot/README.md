# armv7 NativeAOT (unsupported experiment)

A way to produce a NativeAOT `darklang` binary for 32-bit ARM (`linux-arm`) that runs on devices
with an older glibc.

**This is not a supported build, and nothing here runs unless you run it by hand.** CI never
touches it, `scripts/dev/build` never touches it, and no shipped artifact contains any of it. If
you're not deliberately building for armv7, this directory does nothing and you can ignore it.

`linux-arm` ships as ReadyToRun like it always has. That's unaffected by anything in here.

---

## Don't promote this without reading the next paragraph

The shim narrows absolute timestamps to a 32-bit `time_t`. **Anything built this way computes
wrong dates after 19 January 2038.** Durations (sleeps, timeouts) are fine, since they're small
either way, but wall-clock time is not.

That's an acceptable trade for reaching a specific old device you control. It is not acceptable
for anything shipped to users, which is why this lives here instead of in the release matrix.

## Why a shim is needed at all

.NET 10's 32-bit ARM NativeAOT runtime references 22 glibc functions whose names end in
`_time64`. They only exist from glibc ~2.34, part of the 2038 work: 32-bit platforms had to grow
64-bit-timestamp variants of every time-related call.

Older armv7 devices are commonly on glibc 2.31, which squeezes from both sides. You can't link
against the old glibc (the symbols aren't there), and linking against a new one produces a binary
the device refuses to start.

`time64-shim.c` defines all 22, each forwarding to its ordinary 32-bit equivalent and converting
structs at the boundary. The resulting binary has a glibc floor of 2.29.

## Building it

Needs the cross toolchain: clang, an armhf sysroot, lld, and zig. The release image
(`Dockerfile.aot-release` at the repo root) has all of it; the dev container does not.

    docker build -f Dockerfile.aot-release -t dark-aot-release-base .
    docker run --rm -v "$PWD:/root/app" -w /root/app \
      -e HOME=/root -e IN_DEV_CONTAINER=true \
      dark-aot-release-base bash -c '
        git config --global --add safe.directory /root/app
        ./experiments/armv7-aot/build'

The `safe.directory` line is needed because the container runs as root against a checkout you
own, and the build reads the git hash for the version string.

The result lands in `clis/`. It needs a seed database at `rundir/data.db`, same as any release
build, so run this after a normal build has produced one.

## Verified once, on real hardware

Built this way, the binary runs on an armv7 device with glibc 2.31: the interactive workbench
starts, the REPL evaluates, and HTTPS works. Warm commands land around 0.33-0.89 s on a 1 GHz
Cortex-A7, roughly 7-9x slower than x64, which is about what the hardware predicts. First run
takes ~28 s because it applies the package ops.

Two things that cost time and are worth not rediscovering:

- **`struct __stat64_t64` is 112 bytes, not 120.** The trailing `__glibc_reserved4/5` fields in
  the header live in a branch that isn't taken under `__USE_XOPEN2K8`. Including them overran the
  caller's buffer and tripped the stack protector at runtime, long after a clean build. There's a
  `_Static_assert` on the size now.
- **Find missing symbols with a diff, not a loop.** Each build round is 10+ minutes; comparing
  `nm` output of the runtime archives against the sysroot finds the whole set in seconds.

## What was tried and doesn't work

A fully static musl build looks like the obvious escape hatch, since musl has had 64-bit time
everywhere from the start and a static binary doesn't care about the device's libc. It builds and
it runs. It also can't do anything involving crypto: .NET never links OpenSSL, it `dlopen`s it,
and a static binary has no loader to `dlopen` with. Every command dies about five seconds in.

Making that work would mean shipping a musl-built OpenSSL and a musl loader alongside the binary,
which is a parallel userland rather than an afternoon.
