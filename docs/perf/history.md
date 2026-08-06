# Dark performance: what happened, and the numbers

The durable record. Method is in `docs/perf/playbook.md`, what's next in `docs/perf/roadmap.md`.

Rounds: **1** (#5696) and **2** (#5700) were interpreter allocation campaigns, **3** (#5699) was
NativeAOT, **4** is the current branch.

---

## This round: parameterised types

The synchronous fast path from round 2 ran only for types with no type arguments, guarded by
`List.isEmpty typeArgs` in three places, so it never fired for `Option`, `Result` or any
parameterised record. Teaching it to handle type arguments is most of the win.

| workload | before | after | change |
|---|---|---|---|
| `records` | 8.3 KB | **4.26 KB** | **-48.7%** |
| `json` | 13.0 KB | **10.00 KB** | **-23.1%** |
| `dicts` | 15.7 KB | **12.36 KB** | **-21.3%** |
| `strings` | 6.6 KB | 6.45 KB | -2.3% |
| `lists` | 12.0 KB | 11.84 KB | -1.3% |
| `containers` | -- | 3.04 KB | new workload |

An HTTP request now costs **76.31 KB** at 5,269 req/s, p50 2.21 ms (Release, four client
processes). Within the branch, A/B'd in Debug by rebuilding the pre-change type checker, the
construction work alone took a request 99.65 -> 82.29 KB (-17.4%). Per-op: `CreateEnum` 2,016 -> 928, `Option` 2,200 -> 1,112,
`Result` 2,568 -> 1,304, parameterised record 3,464 -> 1,608, `CloneRecordWithUpdates` 2,968 ->
1,040, `dictKeys` 4,400 -> 1,344. Routing a request 333 -> 238 package calls.

None of the six existing workloads moved on the main fix, because they use non-parameterised types
and build few `Option`s or `Result`s. That is why `containers.dark` exists: 9.09 KB/iteration on a
pre-fix build, 6.2 after.

Then a second pass over the same area, after profiling said 43% of the `records` workload is
closures and state machines rather than the value representation: `DvalCreator.enum`, `record` and
`recordUpdate` were unconditional `uply` blocks opening with a `let!`, so every record and enum in
the language entered a state machine. Reading both their lookups with `Ply.trySync` keeps the common
construction out of the builder, and hoisting every helper they use to top level so none of them is
a closure. Ten passes, each found by re-profiling the last. The Release figures are in the table
above; the pass-by-pass work was steered in Debug, where records went 8.82 -> 4.43 KB/iteration,
containers 6.18 -> 3.48, dicts 14.05 -> 12.43 and a request 99.65 -> 82.29 KB. The gate's Debug
budget came down 8.0 -> 7.8 MB.

Also here: `byOpcode` and `byStage` reporting, both already collected and previously discarded; and
`scripts/perf/http` fixed to drive load from several client processes, since one Python process is
GIL-bound below what the server does.

Reverted for measuring flat: exactly sizing the binary readers' collections (3% worse). Closed by
measurement rather than built: freeing the stdlib wrappers, since a warm package call allocates
nothing.

---

## The long arc

Allocation per iteration, Release. Round 3 changes when code is compiled, not what the interpreter
allocates, so it moves this table by less than the noise.

| workload | start | before this round | now | total |
|---|---|---|---|---|
| `recursion` | 13,436 KB | 0.48 KB | 0.45 KB | **~30,000x** |
| `lists` | 3,345 KB | 12.0 KB | 11.84 KB | **283x** |
| `strings` | 684 KB | 6.6 KB | 6.45 KB | **106x** |
| `dicts` | 1,055 KB | 15.7 KB | 12.36 KB | **85x** |
| `records` | 202 KB | 8.3 KB | 4.26 KB | **47x** |
| `json` | 76 KB | 13.0 KB | 10.00 KB | **7.6x** |

Time, Release, 200 iterations. Round 3 is where the time came from.

| workload | start | after round 2 | after round 3 | total |
|---|---|---|---|---|
| `recursion` | 53,389 ms | 501 ms | 385 ms | 139x |
| `records` | 823 ms | 16 ms | 9 ms | 91x |
| `json` | 242 ms | 13 ms | 6 ms | 40x |
| `strings` | 905 ms | 48 ms | 27 ms | 34x |
| `lists` | 2,127 ms | 190 ms | 110 ms | 19x |
| `dicts` | 1,073 ms | 103 ms | 72 ms | 15x |

One-shot `dark eval "1L+1L"`: 214 ms to 31 ms, **6.9x**, almost all of it round 3. About 55% of what
remains is `cli.preMain` -- process start to entering `main` -- so it is no longer interpreter work.

Against other runtimes, same program per iteration: python 0.47 KB, node 1.70 KB, Dark 11.84 KB.
**7.0x node**, from ~700x at the start of round 2.

**Each round in a line.** 0: make the numbers trustworthy -- the profiler was lying in two ways, and
`PackageRefs` makes a binary and a store a matched pair, so package changes cannot be A/B tested.
1: first pass through the interpreter; established that it is allocation-bound and that a Dark
function call cost ~8 KB. 2: the rewrite -- off Ply onto resumable `task`, builtins take arrays,
type symbol table as a struct -- plus the course correction when six workloads finally existed and
the one being tuned against turned out to be the second-cheapest of them. 3: NativeAOT, startup
6.9x and steady state 1.3-2.3x, allocation untouched. 4: parameterised types, plus a wrong-type
resolution bug found while measuring.

Careful with older documents. Round 1's notes quote 107.1 MB and ~868 MB for the reference workload;
those were a *different, lighter* workload, replaced during round 2. Only compare numbers from the
same harness.

---

## Things established that shouldn't be re-derived

Cheap to state, expensive to learn.

**On F# and .NET**

- A `uply` loop allocates per iteration in proportion to the **size of its body**, bind or no bind.
  The win is in **not entering a builder at all**, not in entering a different one: swapping `uply`
  for `task` was measured on this codebase and came out within GC noise, because both are
  struct-state-machine builders. That is why the synchronous fast paths pay and a wholesale
  Ply-to-Task migration does not.
- A completed `Ply` allocates nothing -- it is a struct. The cost is the *builder*, not the value.
- Ply's builder is **not** resumable code, so unlike `task` it allocates in Release exactly as much
  as in Debug.
- F# only reduces resumable-code state machines in **optimised builds**. Record-heavy work looks
  ~2.8x worse in Debug than it is.
- `FS3511: this state machine is not statically compilable` is an *error* in Release here
  (`--warnaserror`) and silent in Debug. The trigger is a bind inside a nested match arm.
- `match d.TryGetValue k with | true, v ->` allocates a `Tuple<bool,'v>` on every lookup, and
  `match a, b with` allocates the pair.
- `let f x = g captured x` is a closure wherever the compiler cannot lift it, and a use inside the
  same function's loop is enough to stop the lift.
- A `let mutable` captured by a continuation becomes a heap ref cell, allocated whether or not the
  branch that needs it runs.
- A lazy `Seq` chain building a collection whose size is known costs an enumerator and a closure per
  stage. `Map.iter` and `Map.foldBack` walk the tree directly, in the same ascending order.
- .NET 10's object stack allocation is **not** quietly removing this work:
  `DOTNET_JitObjectStackAllocation=0` moves the total by 4 KB.

**On Dark's runtime**

- Type names are content hashes, so `type A = { x: Int }` and `type B = { x: Int }` are the **same
  type**. Any test passing a `B` where an `A` is expected is testing nothing.
- Following from that: if a declared type's name equals the value's type name, the declared type
  cannot be an alias, because a value built through an alias carries the *underlying* name. That
  observation is what let most of the type checker move to a synchronous path.
- A **warm** Dark function call allocates **nothing**. Measured by adding 0, 1, 2, 4 and 8 extra
  forwarding hops to a call chain: package calls, frame pushes and instructions all rise linearly
  while total allocation stays byte-identical at 231, and every synchronous `pkg.*` stage reads 0.
  Only the *first* call to a given fn allocates -- frame 511 B, fetch 521 B -- after which both are
  free. An earlier claim here of "120 bytes against 248" for a Dark call is withdrawn; it was
  measuring cold calls, or the callee's body.
- Calling a builtin that has a **type argument** is cheap: ~40 bytes of dispatch per call, and a
  builtin with no type argument measures zero across every stage. An earlier claim here of ~7,470
  was a residual taken across two scripts and is withdrawn.
- The Apply path's *totals* -- `apply.total`, `bi.total`, `lambda.total` -- and `stats.builtinAlloc`
  all bracket across the builtin's `Ply`, so they measure nested execution and can exceed what the
  process allocated. One read 52,393 bytes for a call in an iteration that allocated 11,530. Only
  the fine-grained synchronous stages mean anything.
- `DInt` is 56 bytes regardless of magnitude, because `DarkInt` is a struct DU carrying space for a
  `bigint`.
- An unresolved reference used to carry **no name** into the content hash, so two declarations
  differing only in one collided and a hash-keyed map kept whichever came last. Scripts hit it every
  time, because a script's fns are hashed before its own types are in scope. NOT fixed: the obvious
  fix makes hashes name-dependent, which content addressing forbids. Roadmap 2d and
  `notes/hashing-unresolved-refs-collision-2026-08-19.md`.
- Unused type declarations are dropped before they reach the store, so adding one changes nothing.
  Any experiment that adds a type must use it.
- CRLF is a single extended grapheme cluster, so `"a\r\nb"` split on `"\n"` is one part cluster-wise
  and two char-wise. Any ASCII fast path over strings must exclude CR.
- What a call costs, from `optime.dark` net of its harness baseline. Debug is ~2.5x pessimistic but
  preserves the ordering, so either column ranks the same way:

  | | Debug | published |
  |---|---|---|
  | package fn call, 1 arg | 2.7 us | 1.2 us |
  | builtin call | 1.7 us | 0.5 us |
  | lambda application | 1.6 us | 0.6 us |
  | `++` (after the operator fast path) | 2.2 us | 0.9 us |
  | add two Ints | 0.6 us | -- |

  A lambda application does no argument type-check, no return type check and no TST work, so the
  difference between it and a package call is roughly the package-specific half; the rest is frame
  machinery the two share. A view build makes ~3,292 package calls and ~2,435 lambda applications,
  which is most of its 22 ms.
- **`builtinCalls` over-reports for any per-call arithmetic.** ~4,800 of a view's 8,145 are `Int` and
  `String` operators taken by `tryIntOpDirect`/`tryIntOp`. They are counted but never enter the timed
  path, correctly, since they never reach the builtin machinery. Real builtin calls: ~3,332. The gap
  between the two is a rough measure of what the fast path earns.
- `String.Normalize` already fast-paths ASCII. An `Ascii.IsValid` pre-check in `String.normalize`
  measured as nothing. (And per the CRLF note above, such a check would have to exclude CR anyway.)
- A published binary resolves its data and log directories relative to **its own location**, not
  `DARK_CONFIG_RUNDIR`.
- The first run against an empty store seeds it, which is hundreds of megabytes. Any measurement
  harness needs a throwaway run first.
- Enabling telemetry costs ~6 ms on a one-shot command, so absolute latency needs it off and the
  breakdown needs it on. The two cannot be mixed.

## What a call costs (round 5)

Net of the harness baseline, Debug then published. Debug is ~2.5x pessimistic but preserves the
ordering, so develop against it and quote the published column.

| | Debug | published |
|---|---|---|
| package fn call, 1 arg | 2.7 us | 1.2 us |
| builtin call | 1.7 us | 0.5 us |
| lambda application | 1.6 us | 0.6 us |
| a record field read | 25 ns | -- |

A lambda application does no argument type-check, no return type check and no TST work, so the gap
between it and a package call is roughly the package-specific half; the rest is frame machinery the
two share. A package fetch is ~85 ns. Applying a lambda from a builtin is within 9% of the
interpreter's own Apply.

**`builtinCalls` over-reports for per-call arithmetic**: most of it is operators taken by the fast
path, which are counted but never enter the builtin machinery. Multiplying that counter by a per-call
cost overstates by more than 2x.


## 2026-08-27: both budgets re-baselined off multi-run minimums

Moved here out of `scripts/perf/budget.json`, where it had grown into a paragraph inside a config file.

Debug 7,780,232 (min of 12 runs, 7780232..7861944). Published 7,722,608 (min of 9, 7722608..7755696).
Taken as minimums rather than by `scripts/perf/gate --update`, which records whichever single reading you
happened to get.

The previous numbers were each a single reading: debug 7,763,560 was unreachable by all 12 runs, so the
gate asserted a figure the tree never hit, and published 7,797,328 carried about 1% of unclaimed headroom.

Allocation has MODES on this box: it repeats to 0.0003%-0.03% within a cluster, and the clusters sit
0.3%-1.0% apart. So one reading tells you which mode you landed in rather than what the code allocates,
and the gate's 3% tolerance is what absorbs the band. Take 5+ samples a side before believing any sub-1%
difference.

History worth keeping: published was raised 4.0% for kernel-substrate after the rebase onto main's
records/enums work, then handed most of it back on the rebase onto hash-after-resolution. The
published-vs-debug asymmetry against main is long-standing; the suspect is the Release-only embedded-seed
path, still unproven.


## Moved in from notes, 2026-08-27

Two standing perf facts that were living in `notes/fresh-arch/` where nobody doing perf work would
find them. AGENTS.md already names this file as the home for numbers and facts not worth re-deriving.

---
I spent a while building a case that `push` had a serious performance problem. It does not. The dev
container had `DARK_CONFIG_TRACE_DETAIL=on`, and that alone accounts for every number I measured.

Keeping the note because the trap is worth documenting and I nearly filed the bug.

---

## The numbers

Same call, same store (11,989 ops), same idle box:

| | tracing on | tracing off |
|---|---|---|
| `exportAllSince 0L` (2000-op page) | > 600s | **2.1s** |
| `exportMainOps ()` (all 11,989) | never finished | **5.6s** |
| the same 2000 rows via raw `sqlite3` | 0.014s | 0.014s |

So the export is fine, `push`'s 2000-op chunking is fine, and a full first sync moves about twelve
thousand ops in under six seconds. There is nothing to fix.

## Why it took me so long to see it

Two things pointed away from it.

- 1. **`config/dev` already says `DARK_CONFIG_TRACE_DETAIL=off`**, set by `7339eafc21` on 2026-07-31. So
     grepping the repo told me tracing was off. The running container was created hours before that commit
     and has `on` baked into its environment, which is fixed at create time. The config file and the live
     process disagreed, and I trusted the file.

- 2. **I measured 2.1s early in the session and minutes later on**, which looked like the store degrading
     under me. It was not. `scripts/run-in-docker` forwards any `DARK_*` variable from the host shell into
     the container, so a shell where I had exported `DARK_CONFIG_TRACE_DETAIL=off` produced fast numbers
     and a shell without it produced slow ones. Same code, same data, two orders of magnitude apart,
     depending on which terminal I happened to be in.

There was also a genuine red herring in the middle: leftover test processes at 300% CPU. I caught those and
held off reporting anything, which was the right call for the wrong reason -- I thought I had a contention
problem, and I had a configuration one.

## Fixing it

`config/dev` is already correct, so the permanent fix is to recreate the container (a plain `docker
restart` will not do it; environment is set when the container is created). Until then, any shell can
override it, because `run-in-docker` forwards host `DARK_*` vars:

```
export DARK_CONFIG_TRACE_DETAIL=off
```

## The part that is a real problem

Tracing has no GC, and the cost is not subtle. One night of ordinary work left **15.8 GB in
`trace_fn_calls`** -- 164 traces, roughly 96 MB each, in a store that reached 17 GB. `dark traces delete
--all` plus a `VACUUM` took it back to 152 MB, and it had climbed to 7.3 GB again within a day.

Downstream of that: the relay was OOM-killed while the store was 17 GB, and the client reported `push
failed: network error`. That message is good about what is still configured but says nothing about the
relay possibly being down, which is the first thing to check.

`Tracing.fs` already warns that a single row can reach ~1 GB and that this is why tracing defaults to off
in the shipped binary. What is missing is anything that notices when it happens anyway. `dark status`
reports other standing properties of the store and could report this one -- it is exactly the stale-container
case where the operator has no reason to suspect tracing is even on.


---

Notes from instrumenting `dark` startup on the kernel-substrate branch. Moved out of a comment block in
`Cli.fs`, where the numbers were going stale the moment anyone touched the hot path.

Every figure here is a measurement of one commit against one store. Treat them as a shape, not as constants,
and re-measure before drawing a conclusion.

---

## What the counters are

This branch added three instrumentation seams, all no-ops when telemetry is off:

- `Telemetry.time` spans around the boot phases: `cli.createPM`, `cli.growIfNeeded`, `cli.pmInit`,
  `cli.buildState`, `cli.execute`.
- `Telemetry.counterSnapshot ()`, which reports how many package items a run actually decoded. Emitted
  alongside the spans on purpose: per-item cost and item count are useless separately.
- `RT.InterpreterStatsSink`, a bag every VM registers into when telemetry is on, so the process can total
  instruction / builtin-call / package-call / frame-push counts at exit. A VM is created per
  `executeFunction` and the stats hang off it, so without the sink the object is gone before anything could
  read it.

Read them with `scripts/testing/view-telemetry.py`.

---

## What they said

Same store, same commit, warmed:

```
dark status ...  8,832 instructions,   495 builtin calls
dark help ..... 42,958 instructions, 3,128 builtin calls
```

Both take roughly the same wall time. So:

- Instruction count is not the cost. `help` runs 5x the instructions of `status` and isn't slower.
- Package loading is not the cost either: 36ms.
- Building the execution state is not the cost: `cli.buildState`, 3ms.

What's left is a large FIXED per-process cost, and it is still unexplained. That's the open question.

## Build mode, before you measure anything

Debug 701ms vs release 438ms for `dark status`, warmed, 10 runs. **Measure the release binary.** A genuine
`--aot` build, which `build-release-cli-exes.sh` only does when asked, has never been measured and is the
obvious next thing to try.

An earlier version of this note claimed 3x and ~225ms for release. That came from one uncontrolled run and
does not reproduce; the controlled figure is 1.6x.

## What this branch added to `status` specifically

Worth separating from the fixed cost above, because it's new and it scales with the store rather than being
constant. `Status.summarise` calls `Constraints.pending ()`, which runs an unbounded three-way join over
`locations x package_dependencies x locations` and then one to two more queries per finding through
`shouldFollow -> allChoices`. It also calls `draftRepoints`, which runs a recursive CTE per changed binding.

None of that existed before. If `status` is measurably slower than `help` on a large store, this is where to
look first, and the cheap fix is hoisting `allChoices` out of the per-finding loop.
