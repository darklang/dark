# Shipping NativeAOT instead of R2R: what it would take

What it costs to move the shipped CLI from ReadyToRun to NativeAOT across linux, macOS and Windows on CircleCI, what we'd get, and what could go wrong.

Short version: **the unix side is close to done and worth doing; Windows is a real project; the whole thing is gated on multi-OS CI runners, which is exactly why it was parked in May.** A phased rollout gets most of the benefit without waiting on the hard part.

---

## 1. What we'd get

Measured on this branch, same commit, quiet box, each binary in its own directory, output verified identical first, median of five runs:

| command | R2R (ships today) | AOT | |
|---|---|---|---|
| `version` | 314 ms | **44 ms** | 7.1x |
| `status` | 300 ms | **47 ms** | 6.4x |
| `ls` | 325 ms | **54 ms** | 6.0x |
| `ls Darklang.Stdlib.List` | 387 ms | **90 ms** | 4.3x |

Also:

- **Per-keystroke latency in the interactive prompt: 5 ms** under AOT, comfortably inside a 16 ms frame.
- **Binary is smaller: 27 MB against 46 MB.** Downloads and container images shrink by ~40%.
- **No JIT warmup.** `cli.execute` for `dark version` is 137 ms under R2R and 15 ms under AOT for the same 5,894 instructions. The rest was compilation. This is why short commands gain the most.
- Runtime deps on linux are `libc / libm / libdl / ld-linux` only. No `libe_sqlite3` to ship.

For comparison, the May 2026 measurements on the then-current tree had cold start going 5.55 s -> 1.49 s. Different baseline, same direction.

## 2. What already exists

Nearly all of the plumbing, left deliberately in the tree when CI was reverted:

- `scripts/build/build-release-cli-exes.sh --aot` -- switches the publish to `PublishAot=true`, drops `PublishReadyToRun` and `PublishSingleFile`, and builds the SQLite archives first.
- `scripts/build/build-sqlite.sh` -- builds `libe_sqlite3-<rid>.a` per target using **zig cc**, one toolchain, no per-target sysroot. Already supports `linux-x64, linux-musl-x64, linux-arm64, linux-arm, osx-x64, osx-arm64`.
- `Cli.fsproj` -- `DirectPInvoke` for `e_sqlite3` plus per-RID `<NativeLibrary>` items, all conditioned on `PublishAot=true` so JIT publish is unaffected.
- AOT tuning already chosen: `TrimMode=link`, `InvariantGlobalization=true`, `IlcOptimizationPreference=Size`, `IlcGenerateStackTraceData=false`, `IlcFoldIdenticalMethodBodies=true`.

I ran the existing flag on current code and it worked first time, no workarounds.

## 3. Why it was reverted, and what actually blocks us

From `2ef0a59ad`:

> Cross-OS AOT isn't supported, so until we have CI runners per OS we can't AOT-build the macOS/win
> artifacts that main ships.

That is still the blocker and it is not fixable by cleverness. NativeAOT runs the IL compiler and then the platform linker, so producing a Mach-O binary needs a Mac and a PE binary needs Windows. Today `build-cli` is a single Linux container that cross-compiles every RID, which works for R2R and cannot work for AOT.

**It is not a per-distro matrix.** From `1b32e0827`:

> The current linux-x64 binary dynamically links libc / libm / libdl / ld-linux -- these are present on
> every glibc Linux distro (Ubuntu, Debian, Fedora, RHEL, Arch, etc.). Only Alpine and other musl-only
> distros would need the static-musl variant.

So the axes are (OS family, libc, architecture), not distro version. One glibc build covers mainstream linux; one musl build covers Alpine.

## 4. Per-platform work

### linux-x64 -- essentially done

Validated on current code: 43/43 Dark CLI tests, HTTP client over TLS, `serve` answering through a Dark router, `eval`, a 298,236-instruction script. Builds in the existing container.

One thing to decide: **the glibc version floor.** A binary built against glibc 2.39 will not run on glibc 2.31. The symbols exist everywhere but the versioned symbol requirements do not. Standard fix is to build in a deliberately old base image (the oldest glibc we intend to support) rather than the current `dark-base`. This is a build-image choice, not extra runners.

Fully static would remove the concern, but `1b32e0827` already tried and reverted it: glibc's NSS/dlopen paths need `.so` files at runtime regardless of linking, so static-pie fails to link.

### linux-arm64 / linux-arm -- probably cheap

SQLite archives already build for these via zig. Two routes: cross-compile from the x64 container (ilc supports linux-arm64 cross with the right toolchain packages), or use CircleCI's Arm resource classes and build natively. Native on an Arm runner is lower-risk and CircleCI has Arm Linux available.

### linux-musl-x64 -- one known failure to retry

`1b32e0827` hit `musl-tools` installing `musl-gcc` but not the cross-link CRT files, with both gcc and clang failing. The note says zig might just work now. Worth one afternoon; if it resists, Alpine users can keep a JIT build.

### osx-arm64 / osx-x64 -- needs a Mac runner, otherwise straightforward

SQLite archives already build for both. CircleCI offers Apple Silicon macOS executors. An arm64 Mac can target x86_64, so one macOS job can likely produce both, though building each natively is safer.

Unknown until tried: whether anything in the CLI trips Mach-O-specific trimming or codesigning. Also worth checking whether we need to notarize; an unsigned native binary downloaded from a release will hit Gatekeeper on modern macOS, which is a distribution question we may already have answers for with the JIT build.

### win-x64 / win-arm64 -- the expensive one

This is a project, not a task:

- `build-sqlite.sh`'s `ALL_RUNTIMES` does not include Windows at all.
- `Cli.fsproj` has no `<NativeLibrary>` items for win RIDs.
- `build-release-cli-exes.sh` deliberately filters `win-*` out of AOT builds, because it would fail at link time.

Needed: zig cc producing `.lib` for both Windows arches, matching fsproj items, and a validated publish through ilc plus MSVC `link.exe` on a Windows runner. Plus whatever Windows-specific runtime surprises trimming produces.

## 5. Proposed CircleCI shape

Replace the single `build-cli` job with a fan-out/fan-in:

```
  build-cli-linux     executor: in-container (glibc-floor image), resource_class: xlarge
                      -> linux-x64, linux-musl-x64
  build-cli-linux-arm executor: arm resource class
                      -> linux-arm64, linux-arm
  build-cli-macos     executor: macos (Apple Silicon)
                      -> osx-arm64, osx-x64
  build-cli-windows   executor: windows
                      -> win-x64, win-arm64
  collect-clis        fan-in: attach each workspace, assemble ./clis, publish
```

Each build job: restore the SQLite archive cache, run `build-sqlite.sh --runtimes=<its own>`, then `build-release-cli-exes.sh --aot --runtimes=<its own>`, then `persist_to_workspace` its artifacts.

The archive cache step already existed and was removed in the revert; it can be restored verbatim, keyed on `shasum scripts/build/build-sqlite.sh` (the script pins the SQLite version, so a content change invalidates correctly). Cache it per-OS.

`publish-github-release` changes only in that it attaches several workspaces instead of one.

## 6. Cost

**Build time.** AOT publish is substantially slower than R2R -- ilc compiles the whole program and then links. On this box a single-RID AOT publish took roughly 10-15 minutes against a few minutes for R2R. Fanning out means wall-clock is the slowest job rather than the sum, so PR feedback need not get much worse, but total credit spend goes up.

**Runner cost.** macOS and Windows executors bill at a significantly higher rate than Linux Docker on CircleCI. If every PR builds all platforms this is a real line item. Mitigation: build only the host RID on PRs (which is what the config already does for non-main branches) and fan out to the full matrix only on `main` and tags.

**Maintenance.** Until every RID is AOT, we run two publish paths and have to keep both working.

## 7. Risks

- **Trimming failures surface at runtime, in the specific command that hits them.** The build emits ~48 `IL2070`/`IL2075` warnings, all from `FSharp.Core` reflection. Nothing I exercised failed, but I did not exercise everything, and "everything" now means eight artifacts rather than one.
- **Reflection-dependent features are the danger zone.** Anything doing runtime type discovery, serialization by reflection, or dynamic loading. Our serializers, the package deserializer, and anything in `languageTools` deserve explicit tests per artifact.
- **`InvariantGlobalization=true`** is already set, so culture-sensitive formatting and collation behave differently from a stock .NET app. That is the status quo for AOT builds, but if AOT becomes the only path it becomes the status quo for everyone.
- **`IlcGenerateStackTraceData=false`** drops user-code line metadata. Native stack traces still work via DWARF, but crash reports get worse. Already an accepted tradeoff on the AOT path; worth re-confirming before it applies to all users.
- **glibc floor** (see above) -- easy to get wrong silently, since it only fails on older machines than the build runner.

## 8. The validation gate

Whatever the rollout, the gate should be the same for every artifact, because a trimming regression is invisible at build time:

1. The Dark CLI test suite (43 tests) run against the built artifact, not against a Debug build.
2. A smoke set exercising the reflection-heavy paths: `eval`, `run` on a non-trivial script, `serve` plus a request, an HTTP client call, `ls`/`tree` over package space.
3. Compare output against the JIT artifact for the same commit, not just check for a zero exit code. A failing run is fast, and I mistook a broken binary for a fast one three times during this work.

## 9. Suggested phasing

1. **Now, no CI change.** Keep shipping R2R. Land the glibc-floor base image decision.
2. **Phase 1 -- linux.** Add the archive cache back, AOT-build linux-x64 on main, ship it alongside the other JIT artifacts. Retry musl. Biggest user win per unit of work, no new runner types.
3. **Phase 2 -- linux-arm.** Either cross-compile or add an Arm runner.
4. **Phase 3 -- macOS.** Add the macOS executor. Resolve signing/notarization if it is not already handled.
5. **Phase 4 -- Windows.** The sqlite `.lib` work plus ilc/link.exe validation.
6. **Flip.** When all shipped RIDs are AOT, remove `--aot` (it becomes the only path) and delete the JIT publish branch of the script.

Phases 1-3 cover, I would guess, the large majority of CLI users, and each is independently shippable.

## 10. Open questions

- What is the oldest glibc we want to support? That picks the linux build image.
- Do we already notarize the macOS CLI? If not, AOT does not change that, but it will be more visible if the binary format changes.
- Is anyone on Alpine today, or is musl speculative? It affects whether phase 1 blocks on it.
- Do we care about win-arm64, or is win-x64 enough? Halving the Windows work changes its priority.
- Is the ~40% smaller binary worth anything to us commercially (download size, image size), or is it just nice?
