# Where the next performance work is

The single tracking document for Dark performance work. Replaces `perf-ideas-backlog.md` and the
per-round note piles that preceded it. Update it in place; don't start a new one.

Numbers are measured unless marked *(estimate)*. The difference matters a lot when picking.

**How to measure anything here:**

    scripts/testing/perf-suite              six workloads, allocation per iteration
    scripts/testing/perf-suite --release    the same, against the shipped build
    scripts/testing/perf-http               a real server under concurrent load
    scripts/testing/perf-gate               the CI assertion, against a checked-in budget
    ./scripts/run-cli run scripts/testing/perf-workloads/costs.dark    per-operation cost table

Method, and the traps: `docs/perf-playbook.md`. History and how we got here:
`docs/perf-history.md`.

Only `perf-gate` runs in CI, and only because it's one script and a couple of seconds. The suite and
the HTTP driver are for a human deciding something, not for every build.

---

## Where things stand

Reference workload, Release, whole process: **7.6 MB**, from 211.99 MB at the start of round 2. Per
iteration the list workload is **7.1x node**, past the ~10x goal -- but it is the *cheapest* of six,
so treat it as a best case rather than a summary.

Per iteration, Release: `recursion` 0.48 KB, `strings` 6.6, `records` 8.3, `lists` 12.0, `json` 13.0,
`dicts` 15.7. An HTTP request returning a constant string: **85.4 KB**.

Time is the axis that hasn't moved: 1.4x in Release this round against 28x for allocation. That is
what makes AOT the right next step rather than more of this.

## Next round is NativeAOT

Decided 2026-08-08. It's the right call and it outranks everything below: one-shot command latency
is dominated by `cli.preMain` -- runtime init and JIT -- which is exactly what AOT deletes, and no
amount of interpreter work touches that. It's also already built, on its own branch.

**The one thing to measure rather than assume.** AOT trades steady-state throughput for startup: it
compiles ahead of time, so the JIT can't use runtime profile information or re-optimise hot loops.
Long-running work can come out *slower*. This repo already has a hint in that direction -- round 1
landed `TieredPGO=0` because tiered PGO was hurting throughput here.

So when AOT lands, run `perf-suite --release` and `perf-http --release` before and after, not just a
startup timing. The six workloads are all steady-state loops and are precisely the shape that could
regress. If they do, that's a real tradeoff to make deliberately, not a surprise to find later.

**And re-pin the gate.** `scripts/testing/perf-budget.json` holds a published budget measured against
an R2R build. An AOT binary will not allocate identically, so the gate needs
`perf-gate --published --update` in the same commit, with the new number stated.

Everything below is the queue after that.

## Ranked, with what's known

### 1. The record and enum opcodes stop the interpreter's fast loop

**Measured, biggest known win, design written.** Building a record costs 6x calling a function
(1,507 B vs 231 B); updating one costs 2.3x building one (3,431 B). `CreateRecord`, `CreateEnum` and
`CloneRecordWithUpdates` are three of the four opcodes that stop `runSyncInstructions` and enter the
computation expression. They only need to be async because the builders can need the type store,
which is now cached, so in the common case the `Ply` they return is already complete.

The full design, including why no new DU cases are needed and what to verify, is in the task list
(#69). Records and enums are what real programs are made of, so this is the fast loop being left
constantly.

### 2. Pool the builtin argument buffers further

**Measured ceiling.** Already done per-frame, which took recursion to 0.52 KB/iteration. A
deliberately unsafe whole-VM probe suggested there is a little more, but the per-frame version got
essentially all of it. Low priority now; recorded so nobody re-probes it.

### 3. An HTTP request costs ~104 KB to return a constant string

**Measured, mostly not the runtime's fault any more.** 82% is framing rather than the handler, and
93% is Dark-side interpretation rather than F# marshalling: **333 package calls to route one
request**. That is `Stdlib.HttpServer.routeRequest` splitting paths and walking handler lists, so the
win is now in the standard library, not the interpreter. Worth doing -- it's what every Dark web
service pays -- but it's a different kind of work.

### 4. Unbox scalar Dvals

**Partly done, representation untouched.** Small integers and both booleans are interned, so
`1 + 1` allocates nothing. But `DInt` is 56 bytes because `DarkInt` is a struct DU carrying space
for a `bigint`, so any integer outside -128..1023 still costs that. Splitting the representation, or
making `Infinite` hold a reference, is the real fix. *(estimate: moderate win, wide blast radius)*

### 5. Move type checking to compile time

**A real project, weeks not days.** Ablation said the checking half is worth -20% allocation and
-14% wall. Note this round already took a large bite out of the *runtime* cost by caching and by
answering the common cases synchronously, so the remaining prize is smaller than when it was first
scoped -- re-measure before committing to it. The place to put a static checker already exists
(`applyAddFn`), the store is content-addressed so a verdict never goes stale, and the dependency DAG
for invalidation is already a table. Most of the risk is soundness, not plumbing.

Full design: `docs/perf-compile-time-typechecking-design.md`.

### 6. NativeAOT for the shipped CLI

Promoted -- see the top of this document. Full report: `docs/perf-aot-shipping-report.md`.

### 7. Startup, and one-shot command latency

**Measured, deprioritised because AOT is the lever.** `dark eval "1L+1L"` spends most of its time in
runtime init and JIT, which is what AOT deletes. The part that survives AOT: **46 package functions
load to add two integers**, one SQL query each, and **a SQLite point lookup costs ~0.4 ms** against a
local file, which should be tens of microseconds. It doesn't vary with payload, so it's
per-statement overhead -- Fumble builds a fresh `SqliteCommand` per call, so the statement is
re-prepared every time. WAL, `synchronous=NORMAL` and pooling are already on.

### 8. JSON

**Now among the most expensive per iteration.** The fixed cost per `Json.parse` is down from 8,501
to ~3,700 bytes but is still the bulk for small documents. Known remaining items, all small: the
converter's `match typ, j.ValueKind with` allocates a pair per node (F# builds the tuple and boxes
the enum), record fields go into an `FSharpMap`, and the `JsonDocument` is never disposed so its
pooled buffers are never returned (~350 bytes a parse -- measured, and *not* worth the
use-after-dispose hazard on its own).

### 9. Dicts and records both use `Map<string, Dval>`

`Dict.set` costs ~370 bytes, most of it `Map.add` rebuilding the tree path, which is inherent to an
immutable dict. A cheaper small-map representation would help dicts *and* records, since `DRecord`
holds its fields the same way. Big change; no design yet. *(estimate)*

### 10. The rest of the codebase is still on Ply

~400 `uply` sites outside the interpreter loop. The cost is the *builder*, not the value -- a
completed `Ply` is a struct and allocates nothing -- so the ones that matter are those entering a
builder to hand back something they already have. Several such sites have each been worth 5-20% of a
workload. Consider a `ValueTask` builder.

## Smaller, known, unowned

- `run-in-docker` hangs after the command finishes (`cat <&0` waits for EOF); pass `< /dev/null`.
  Every later command in the clone crawls until it's killed. Fixing it properly is a small job.
- Re-enable the `CliTraces` suite once its network call is stubbed.
- Decide what `dark version` should do when the network is slow or absent -- it makes a network call
  that costs ~700 ms.
- Unify `callBuiltinResolved` and `callPackageResolved`: same five steps, different parameter and
  outcome types. Needs `BuiltInParam` and `PackageFn.Parameter` to share an interface.
- Make builtins opt-in, as installable extensions. Would cut startup and the builtins table.
- Mine the telemetry corpus: every checkout's `rundir/logs/telemetry.jsonl` has thousands of real
  runs, which could retarget the campaign against what people actually do.

## Considered and declined

- **Moving the gate to the `build-cli` job**, so it measures the artifact users actually download
  rather than the solution publish in `build-backend`. `build-cli` does run migrations and
  `reload-packages`, so it would work. Declined because both are Release builds of the same code and
  the difference is packaging, so there is no measured reason to expect different allocation --
  and moving a CI step for an unmeasured reason is exactly what this campaign learned not to do.
  Revisit if the release script ever starts passing different publish flags (trimming, single-file,
  AOT), because then the binaries genuinely differ.

## Closed, so nobody re-opens them

- **Inline caching at call sites.** The point was skipping the per-call type check. Memoizing
  container `ValueType`s took those stages to 0 B/run; there is nothing left to cache.
- **Whether a package callee should inherit the caller's type symbol table.** Was only ever a
  performance question via inline caching. Still an open *design* question, but not a perf one.
- **Argument lists.** Done: builtins take an array, reused per frame.
