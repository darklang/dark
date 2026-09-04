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
## Effect/permission system (2026-09): 7.80MB -> 11.67MB -> 9.5MB

- The added ~3.8MB is outside the per-operation path: most is startup, with a small per-run setup
  cost. Proof: byOpcode for the measured loop stays ~2.5MB, and a 10x-iteration run leaves the
  `SetTreeNode<String>` allocation ticks flat (21 -> 22) while only baseline `FSharpList<Dval>`
  arg-list allocation grows. So the per-op path the gate guards is unchanged.
- The per-op permission path is already lean and must stay so: pure builtins short-circuit on an
  empty effect set (`Interpreter.fs` ~962), allow-all package frames skip the `Access.restrict`
  entirely (`~1282`), and the fn-reference access stamp is memoized in a `ConditionalWeakTable`
  (`LoadVal` measured 2,288 bytes across 113k loads).
- The startup cost is the bundled-hash set (`hashesOwnedBy "Darklang"`, ~4234 entries),
  the instance/run policy load, and per-run `Access` setup -- spread across the subsystem, no single
  hotspot. The harmful/deprecation set is empty here (0 rows) and is not involved.
- A clean main baseline is blocked on this branch: the serialized package format differs, so a main
  binary cannot read this clone's package DB. Attribution above rests on the scaling test, not a
  side-by-side. If you want the exact split, build main in a second clone with its own DB.
- The published budget remains the previous baseline (`7,641,184`) and has not been remeasured on
  this branch. Debug allocation does not predict published/AOT allocation; re-pin it with
  `scripts/perf/gate --published --update` once a release build is available.

- Follow-up A/B measurements in this clone (same policy: `permissions allow all`; a deny-all policy
  short-circuits the workload and reads as a false win). Three changes, in order of effect:
  - `hashesOwnedBy` returns a `HashSet<string>`. F# `Set.ofList` over ~4200 strings allocates
    O(n log n) tree nodes (`SetTreeNode<String>` was 10% of allocation ticks).
  - The guest state no longer materializes the bundled set as a `Map<Hash, Policy>` (one
    `Map.add` per member, 12% of ticks); bundled membership is a hash-set check behind a
    lookup function, and the interpreter memoizes the resulting policy per fn.
  - `LoadVal` stamps a fn-reference constant only when it is not consumed by the next
    `Apply` on the same register; partial application stamps instead. That removes the
    `ConditionalWeakTable` probe from every named call (allocation was already ~0).
- Trap: `byStage.bi.total` counts everything nested inside a builtin, and a `dark run`
  script executes inside `cliParseAndExecuteScript`, so it reads as ~200 bytes per
  builtin call when it is really the whole run. Per-call attribution needs the sub-stages.
- Second simplification round (same day): 9.5MB. Allocation-free `Access.check` loop
  with a one-restriction allow-all fast path; `Request.ofAmbientEffect` moved the
  effect→request match out of the gate; posix and host arms collapsed into helpers
  (no per-op effect).
- After the rebase onto main's VM pooling (2026-09-03): debug 10.3MB, published 10.0MB against
  main's 7.2MB / 6.85MB. A 10x-iteration profile showed `FSharpList<Restriction>` scaling with
  the workload: `VMState.reuseFor` reset the pooled root frame with `Access.start Policy.denyAll`,
  a fresh list per lambda application. `Access.denyAll` is now one shared immutable value:
  debug 9.2MB, published 9.1MB, both pinned. The remaining ~2MB is startup (strings from the
  bundled-hash query, policy load) and does not scale with the workload.
