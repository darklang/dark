# Where the next performance work is

The single tracking document for Dark performance work. Replaces `perf-ideas-backlog.md` and the
per-round note piles that preceded it. Update it in place; don't start a new one.

Numbers are measured unless marked *(estimate)*. The difference matters a lot when picking.

**How to measure anything here:**

    scripts/perf/suite              six workloads, allocation per iteration
    scripts/perf/suite --release    the same, against the shipped build
    scripts/perf/suite --record     append a run to benchmarks/results/history.jsonl
    scripts/perf/http               a real server under concurrent load
    scripts/perf/gate               the CI assertion, against a checked-in budget
    scripts/perf/checks             by-hand semantics and error-message checks
    scripts/perf/crosslang          the same workload in node and python
    scripts/perf/alloc-profile           allocation by type name
    ./scripts/run-cli run scripts/perf/workloads/costs.dark    per-operation cost table

Method, and the traps: `docs/perf/playbook.md`. History and how we got here:
`docs/perf/history.md`.

Only `scripts/perf/gate` runs in CI, and only because it's one script and a couple of seconds. The suite and
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

**Measured 2026-08-08. Both checks done; numbers in `history.md`, round 3.**

The throughput worry was half right. The six suite workloads did not regress, they came out
1.3-2.3x faster. But `perf/http` under sustained load (20k requests, ~11s) has R2R ahead in 3 of 3
paired runs by 0.3-5.6%, with latency moving the same way. A short 2k-request run shows a dead
heat, so the cost only appears once the JIT has time to finish tiering, which is the mechanism
predicted here. The trade is a few percent of sustained server throughput for 4.6-8x on one-shot
commands. Right side of the trade for a CLI; worth re-measuring if `serve` ever becomes the main
way Dark runs.

**The gate was deliberately not re-pinned.** An AOT binary does allocate differently, as predicted:
8.2 MB against R2R's 7.7 MB on the reference workload, reproducible, and `gate --published` fails
against it. But that gap is startup, not the body (`suite` differences startup out and shows
allocation unchanged), and CI's gate runs against the plain Release publish from `build-backend`,
which the AOT work doesn't change. Re-pin to 8.2 MB when the published build actually becomes AOT,
not before.

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

**A real project: weeks, not days.** The plumbing is easy and the middle is a type checker for a
polymorphic language. Ablation put the prize at -20% allocation and -14% wall, but that was measured
before this round cached and synchronised much of the same work, so re-measure before committing.

**Runtime type checking is two jobs in one code path, and only one can move.** *Checking* asks
whether an argument matches its declared type; making `checkFnParam`/`checkFnResult` no-ops is where
the -20% came from, and everything still runs. *Instantiation* binds type variables into the symbol
table so `'a` has a meaning inside the frame; remove it and the CLI does not start, because
`Json.parse` has no concrete type to parse into. So this is a split, not a deletion.

**Where the static checker goes.** `applyAddFn` in `LibDB/PackageOpPlayback.fs` already compiles
package code at save time and stores `rt_instrs` beside `pt_def`; a checker is another pass there and
its verdict another column. Content addressing means a verdict never goes stale -- the hash *is* the
definition -- which removes the invalidation problem that makes this miserable elsewhere.
`package_dependencies` already gives topological order and reverse reachability.
`LibParser/Validation.fs` is the model for the pass itself: same shape, one stage earlier.

**The gradual boundary.** Three states per item -- `Checked`, `Unchecked`, `Failed` -- and a call
skips its runtime argument check only when caller and callee are both `Checked`. Checks stay
wherever a value enters from outside a verified region: scripts and `eval`, deserialization
(`Json.parse`, DB reads, request bodies), builtins returning `Unknown`, and any `Unchecked` callee.
This degrades correctly: a store where nothing is checked behaves exactly as today, and each item
that passes removes checks from its own call sites with no flag day.

**Where it will hurt.** The checker must under-approximate: anything it cannot prove is `Unchecked`,
so being wrong costs performance rather than correctness -- the opposite direction trades a good
error message for undefined behaviour. Static `TypeReference` and runtime `ValueType` are different
lattices that meet in `unifyValueType`, and the existing `TypeChecker` should not be assumed reusable
for a value-free version. And most calls are into signatures containing a type variable, so this
needs real inference over type variables; a design that fast-paths the monomorphic case is optimising
the small half.

**Phasing**, each step independently useful and revertable. 1: split checking from instantiation in
the interpreter, no behaviour change, so "skip the check, keep the binding" becomes a condition
rather than a refactor. 2: write the checker and run it over the whole package set in CI reporting
only -- how many check clean, how many it declines, how many it calls wrong (expect both real errors
and checker bugs). This is the phase that says whether the idea is viable, and it ships nothing.
3: store the verdict, wire the runtime skip behind a flag defaulting to off, measure. 4: default it
on. Phases 1 and 3 are days each; phase 2 is the project.

**Decide before starting:** whether `Failed` blocks a save (probably warn first -- early `Failed`
results will mostly be checker bugs); that the checker runs on already-resolved references, since
items are global but name resolution is branch-scoped; and whether the LSP is the better reason to
build it, since type errors while typing may be worth more than the allocation.

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
- Runtime call stacks name package functions by content hash, which makes every error harder to act
  on than it needs to be. Not a performance problem, but it is in the same code.

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
