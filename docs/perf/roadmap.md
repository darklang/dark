# Where the next performance work is

The single tracking document for Dark performance work. Update it in place; don't start a new one.
Numbers are measured unless marked *(estimate)*.

    scripts/perf/suite              six workloads, allocation per iteration (--release for the shipped build)
    scripts/perf/http               a real server under concurrent load (-p client processes, default 4)
    scripts/perf/gate               the CI assertion, against a checked-in budget
    scripts/perf/checks             by-hand semantics and error-message checks
    scripts/perf/bench              repeatable CLI timing, with an A/B mode
    scripts/perf/alloc-profile      allocation by type name
    scripts/perf/crosslang          the same workload in node and python
    scripts/perf/keystroke          what one keypress costs in the interactive CLI (needs a TTY)
    ./scripts/run-cli run scripts/perf/workloads/costs.dark      per-operation cost table
    ./scripts/run-cli run scripts/perf/workloads/workbench.dark  ms to build each workbench view

Method and traps: `docs/perf/playbook.md`. Numbers round by round: `docs/perf/history.md`.

Only `gate` runs in CI. The suite and the HTTP driver are for a human deciding something.

---

## Where things stand

Per iteration, Release: `recursion` 0.45 KB, `containers` 3.04, `records` 4.26, `strings` 6.45,
`json` 10.00, `lists` 11.84, `dicts` 12.36. An HTTP request: **76.31 KB** at 5,269 req/s, p50
2.21 ms.

The two easy classes -- a wrong data structure on a hot path, and a computation expression around
work that never awaits -- are largely spent. What remains is a representation change with a wide
blast radius, or standard-library work. Expect smaller allocation wins than earlier rounds.

Allocation, execution speed and latency all count now. A change that only moves wall-clock is a
legitimate win, which was not true in the first two rounds.

**Improve the instrument when the instrument is the limit.** `recursion` reads 0.20-0.48 KB across
runs of the *same* binary, because it is a difference of two large startup numbers. Before chasing
anything that small, fix the measurement.

**AOT, measured against R2R on the same commit:** startup 6.9x faster (214 ms to 31 ms), steady
state 1.3-2.3x on all six workloads, allocation identical. The open gap is that `gate` measures the
solution publish, not the AOT artifact users download. Low severity while allocation is identical
between them, but nothing measures the shipped artifact.

---

## Round 5: the interactive CLI, which nothing has ever measured

Rounds 1-4 measured allocation on interpreter workloads. Every instrument we have takes a fixed
script and reports bytes. None of them touches the thing a person actually waits for, which is the
CLI redrawing after a keypress, and it turns out to be the slowest thing we ship.

**One arrow key costs ~283 ms** (`scripts/perf/keystroke`, 120x40, median of 12, spread 281-356).

Size changes the answer by 3x, and not smoothly: 80x24 is ~95 ms, 120x40 is ~284, and 280x60 is only
~317. The step is between 80 and 120 columns, where the workbench starts laying out all its panes, so
this is a per-pane cost rather than a per-cell one. **Never compare two runs at different sizes.**

Building the view is most of it. `scripts/perf/workloads/workbench.dark` renders each view with no
terminal involved, repeats to ~3%, and can be profiled:

| view | ms/render |
|---|---|
| Home (0) | 179 |
| Matter (2) | 120 |
| the rest (1, 3-9) | 151-178 |

So ~150 ms of the ~283 is building the view, and the remaining ~130 is reading the key, diffing the
frame and writing it -- unattributed so far.

### Two instruments, and what they cost to trust

`workbench.dark` is the one to work against: pure, loopable, repeatable, no TTY. `keystroke` is the
end-to-end check that the loop actually got faster. Traps that each produced a confident wrong number
before being handled, all written into the scripts:

- Measure to the **last** byte of the redraw. First-byte latency reads ~95 ms where the screen takes
  ~300, and the gap is the whole problem.
- Handle `eof` on every `expect`. A CLI that died reads as a keypress that produced no output.
- The workbench's cost is **data-dependent**. Home's detail pane only does its expensive work when
  the cursor is on a commit row and that commit isn't cached, so a view-level A/B can move by 10x
  between two stores. Measure the function, not the frame, when the function is what changed.

### Done

**A commit's summary loaded the whole commit.** Home's detail pane shows twelve of a commit's named
changes; it called `getCommitOps`, which turns every op into a Dark value. Against the init commit
(~11,700 ops) that is `commitChangeLines` at **1,648 -> 330 ms**, same output.
`scmGetCommitNamedOps` filters and caps F#-side and converts only what will be shown.

### Where the 283 ms goes

Measured per phase, headless, 120x40, `initialState` with every view gated on:

| phase | ms |
|---|---|
| `handleKey` -- the state transition | 71 |
| `viewAtSize` -- building the view | 172 |
| `preparePresentAtSize` -- diffing and encoding the frame | 4 |
| unattributed (stdin, the terminal write, loop) | ~36 |

The frame diff is not the problem and never was. Two things are.

**Done: `visibleViews` was recomputed ~28 times per sidebar build.** `visibleDests` took `state` and
called `visibleViews` itself, and every caller was inside a loop over the nine workspaces, so
building the sidebar recomputed the same 13-element list once per workspace in `visibleWorkspaces`,
again per workspace in `sidebarRows`, and again per row in `workspaceEntry`. Threading the computed
list through (`visibleDestsIn`, `visibleWorkspacesIn`, `workspaceEntryIn`) computes it once.

| | before | after |
|---|---|---|
| `sidebarRows` | 66 ms | **18** |
| `handleKey` | 71 ms | **23** |
| `viewAtSize` | 172 ms | **120** |
| keypress, end to end | 283 ms | **198** |

The view build fell too, which confirms the sidebar is a real share of every view.

**`sidebarRows` is still built at least twice per keypress.** It is the whole of
`handleKey`'s 71 ms: `focusRight` alone measures 0 ms, and an unhandled key (F12) costs the same 70
ms as a handled one, so none of it is the action. `handleSidebarKey` opens with
`let rows = sidebarRows state 0 0`, and `renderSidebar` builds the same rows again during the view.
`syncSideSel` makes a third call on the transitions that run it.

Building it once per keypress is the obvious win and worth ~66 ms. The better question is why ~15
rows of pure list-and-record work cost 66 ms at all, with no DB call anywhere in it. That number is
the one to chase; it likely indicts something shared rather than the sidebar.

**`handleSidebarKey` computes what keys you didn't press.** `let` is eager, so `openSel` and
`collapseSel` are evaluated on every keypress, and `openSel` reaches `goTo`, which calls
`itemsForView` and loads items from the DB. Measured on its own, `goTo` on row 0 is 70 ms. It does
not show up in `handleSidebarKey`'s total, which is within noise of `sidebarRows` alone -- so
something is not evaluating it, and *that* wants understanding before anything is changed here.
Making both lazy (nested functions rather than values) is correct regardless of what the measurement
turns out to mean.

### Open, ranked

**Why ~15 sidebar rows cost 66 ms.** No DB, no I/O; pure interpreted list and record construction.
This is the largest single number in the keypress and the least explained.

**~150 ms to build one view is the floor for every keypress.** No view is cheap, and the cheapest
(Matter, 120 ms) is not much cheaper than the dearest. That flatness suggests a shared cost -- layout,
span composition, or the per-row fitting in `Cli.Tui.Text` -- rather than anything view-specific.
Profile one view amplified before picking. Note the sidebar is part of every view, so the item above
may be most of this one.

**A record update is not the problem.** 2,000 updates of the 29-field workbench `State` take 22 ms,
about 11 us each, so the value-representation item from earlier rounds is not what makes a keypress
slow. Worth knowing before someone reaches for it here.

**The remaining 330 ms of `commitChangeLines`** is deserializing ~11,700 op blobs to discover which
are `SetName` or `Deprecate`. Bounding it needs the op kind readable without deserializing, which is
a schema question.

**Redraw on every keypress at all.** An arrow key moves a cursor; it does not change most of the
frame. Whether the diff already exploits that, and what it costs when it does, is unmeasured.

### Tooling gaps found while doing this

- `alloc-profile` cannot run: `dotnet-trace` is not installed in the container, and the script fails
  with `command not found` rather than saying so.
- **`gate` does not restore a fixture, and the debug store drifts.** The same commit measured
  7,780,170 bytes and then 8,530,000 an hour later with no code change, because the store grows with
  every package reload. `bench` restores a byte-identical store for exactly this reason. A debug
  gate comparison taken across a work session is not a comparison.
- **`scripts/dev/build` can skip a rebuild it needed.** It compares by content, so after switching
  branches to something built earlier in the session it reports "nothing has changed since the last
  successful build" while the binary on disk belongs to the other branch. Deleting
  `rundir/build-index.json` forces it. This produced a `FnNotFound` that looked like a code bug.

## Open, roughly ranked

### Startup: ~19 ms of the 21 is ours, and it is before `main`

Answering the question this item carried for two rounds: **no, 20 ms is not normal for a NativeAOT
binary.** A hello-world AOT built with the same SDK starts in **1.8 ms** (median 1.9), against
`/bin/true` at 0.5. Whatever our binary spends, it is not the runtime's floor.

Measured on the shipped AOT binary with an argument it rejects immediately, which is the closest
thing to a no-op invocation available (`--version` is useless for this: it makes a network call and
takes 320 ms):

| event | median |
|---|---|
| whole process | 21.8 ms |
| `cli.preMain` | **20.5 ms** |
| `cli.total` | 11.0 ms |
| `cli.execute` | 6.0 ms |
| `cli.builtinsInit` | 2.0 ms |
| `sql.total` | 2.0 ms |
| `cli.extractResources` | 1.0 ms |

`preMain` and `cli.total` overlap, so they do not sum to the process time; read them as two views.

Inside `preMain`, `strace` finds a single **15.9 ms window with zero syscalls**, so it is
computation, not I/O, and not page-faulting the 28 MB image. It is **not the GC**, though the window
opens right after `sched_getaffinity`: `DOTNET_gcServer=0`, `GCHeapCount=1` and `GCHeapHardLimit`
all match the default across nine runs each.

What is left to check: eager static initialisation. F# module-level values run on first access, and
`cli.builtinsInit` at 2 ms shows the builtins table alone is measurable, so the natural suspect is
the rest of that class. A profiler that can see inside a no-syscall window is the missing tool.

**Worth keeping in proportion.** 21 ms for a no-op is already good, and runtime performance matters
more. This is written down because the question was open for two rounds and is now settled, not
because it is the next thing to do.

### Package deserialization: real, but unmeasurable -- parked

A one-shot eval loads 59 package items for 1.03 MB, ~17.5 KB each and 24% of the command's
allocation, from an average 1,129-byte blob: ~15x expansion into the object graph. The load path
around it is lean.

Parked because the instruments cannot resolve it: `alloc-profile` yields 41 ticks total on a
load-dominated run, and items are cached per process so the work cannot be looped in-process.
Reviving it needs a harness driving the loader directly, a finer allocation provider, or counters
inside the deserializer. One guess has already been tried and reverted.

### Record and enum construction still enters three builders per value

The top runtime item, measured. An amplified `records` workload (60,000 iterations, 4,487 allocation
ticks, so the profile actually resolves) says **43% of its allocation is closures and async state
machines**, concentrated in the construction path:

| enclosing method | share |
|---|---|
| `enum` | 14.8% |
| `recordUpdate` | 8.2% |
| `resolveEnumType` | 4.8% |
| `resolveRecordType` | 4.5% |
| `record` | 4.0% |
| `list` | 2.4% |

For comparison the entire `Map<string, Dval>` family -- the thing a value-representation change would
address -- is about 18%.

Two causes, both continuations of this round's theme.

**The enclosing functions are unconditional `uply` blocks.** `DvalCreator.enum` and `recordUpdate`
open with `let! ... = resolveEnumType/resolveRecordType`, so a builder is entered on every call. This
round put synchronous fast paths *inside* the field loops; the functions wrapping those loops still
build a state machine. The chain is three deep -- `enum` to `resolveEnumType` to `resolveType` --
for a lookup that is a cache hit in steady state.

**`resolveType`'s cache is behind `List.isEmpty typeArgs`.** It already has the right shape:

    if List.isEmpty typeArgs && (cacheFor types).TryGetValue(typeName, &cached) then
      Ply cached

That is a fourth instance of the condition this round removed from three other places, and it means
the cache never serves a parameterised type. Unlike the other three it is not simply dead: the
returned value includes a type-argument mapping, so caching by name alone would be wrong. The fix is
the same shape as the round-4 one -- cache what does not depend on the arguments, derive the mapping
synchronously -- rather than deleting the condition.

**Done, five passes, Debug, three readings at each step:**

| workload | round start | now | change |
|---|---|---|---|
Debug, which is what the pass-by-pass work was steered by:

| workload | round start | now | change |
|---|---|---|---|
| `records` | 8.82 KB | **4.43** | **-49.8%** |
| `containers` | 6.18 KB | **3.48** | **-43.7%** |
| HTTP request | 99.65 KB | **82.29** | **-17.4%** |
| `json` | 17.40 KB | 15.23 | -12.5% |
| `dicts` | 14.05 KB | **12.43** | **-11.5%** |
| `strings` | 6.94 KB | 6.61 | -4.8% |
| `lists` | 12.20 KB | 11.93 | -2.2% |

Release, measured against a CLI built from this branch's head, which is what ships and what
`history.md` records: records 8.3 -> **4.26** (-48.7%), json 13.0 -> **10.00** (-23.1%), dicts
15.7 -> **12.36** (-21.3%), strings -2.3%, lists -1.3%, containers **3.04** (new).

Closures fell from 43% of the records workload to 7%, and the profile's tick count from 4,487 to
2,571 for identical work. A real HTTP request went 99.65 -> 89.26 KB after three of the five. The
gate's Debug budget came down 8.0 -> 7.9 MB. On a real HTTP request,
A/B'd by rebuilding the pre-change type checker, the first half alone was worth 99.65 -> 95.91 KB;
throughput is unchanged at ~4,080 req/s, which is expected -- allocation is not what limits
throughput here.

It took three passes, each found by re-profiling the one before, and none of them was the step
originally planned.

1. `enum`, `record` and `recordUpdate` were unconditional `uply` blocks opening with a `let!`.
   Reading both their lookups with `Ply.trySync` keeps the common construction out of a builder.
   Worth -11%.
2. The next profile showed an 11.3% entry, `afterResolve` -- the local helper step 1 had just
   introduced. Marked `inline`, but it closed over `types`, `threadID` and `tst`, so F# allocated a
   closure per construction anyway. Hoisting the three helpers to top level, taking everything
   explicitly, was worth another -12.5%: more than the change it was cleaning up after.
3. The next profile showed `resolveEnumType` and `resolveRecordType` at 11.8% combined, both trivial
   `uply` wrappers around `resolveType`, plus `finish` at 2.5% -- another local helper left behind by
   step 2. Same treatment: -11.4%.
4. With closures down to 11%, the next profile put `DvalCreator.list` and `dict` on top: both folded
   a lambda over a tuple accumulator, so a closure and a tuple per element. Top-level recursions
   with a struct-tuple result instead. records -8.6%, containers -12.4%, json -5.5%.
5. Closures then being spent at 7%, the profile turned to reference tuples, the largest at 9.4%:
   `unifyTypeArgsSyncVT` used `match declared, actual with`, which allocates the pair, once per type
   argument on the path every parameterised value takes. Nested matches instead. This is the only
   pass that moved *every* workload -- containers -15.3%, records -5.3%, strings -4.0%, dicts -1.8%,
   and dicts had not moved all round.

6. The return tuples. `resolveEnumType` and `resolveRecordType` returned reference `Tuple3`s at
   4.4% combined; both are struct tuples now. records -6.4%, containers -1.6%, HTTP -5.4%.

   This one looked like it cost something: `lists` +1.2% and `strings` +1.8%, repeatable across
   three readings. It prompted a hypothesis -- that a struct tuple crossing a `Ply` boundary
   enlarges the state machine on the *async* path, so struct tuples would pay only where the
   synchronous path dominates.
7. `checkEnumFields` returns a struct tuple too, which was chosen as the test of that hypothesis
   because it is the same shape of change on the same paths. **The hypothesis did not hold.**
   `lists` went 11.97 -> 11.70, below even its pass-5 value, so pass 6's apparent regression was
   recovered rather than compounded. records -5.8%, containers -4.0%. `strings` drifted up again
   but its spread (6.75-6.94) overlaps its pass-5 spread, so that one is unresolved rather than
   real.

8. `checkRecordFields` and `updateRecordFields`, the last reference `Tuple3`, at 6.7%. records
   -4.1%, strings -4.1%, HTTP -2.5%, `lists` +1.9%. Mixed, and HTTP decided it.

**What is left is the value representation.** After eight passes the profile is
`FSharpList<Dval>` 18.0%, `MapTreeNode` 14.5%, `Option<Dval>` 8.8%, `FSharpMap` 7.4%, and the
`Tuple2<string, Dval>` pairs a Map is built from -- roughly 34% in the Map family alone. Those are
not accidents of how the code is written; they are what a `DRecord` and a `DDict` *are*. Closures are
down to 10% and no single non-representation entry is above 7%, so the flat-profile rule applies:
stop optimising this file and change the representation, or stop.

**On `lists`:** it read +1.2% at pass 6, -2.3% at pass 7, +1.9% at pass 8, always with a tight
within-sweep spread. That is build-to-build variance, not a trend; see the playbook note.

The pattern is worth stating plainly, since it caught the same mistake three times: **a local helper
on a hot path is a closure however it is annotated**, and each fix reveals the next one only if the
profile is re-read. Do that before assuming what is left.
Both lookups are read with `Ply.trySync`, so a construction whose type is cached and whose fields
unify never enters a builder. The slow paths await the *same* `Ply` rather than recomputing, so
there is one implementation of each step and no twin. The case lookup and arity check moved into
`enumCaseFields`, shared by both paths, so the errors read identically either way.

Still to do, from the same profile: the `List.isEmpty typeArgs` condition on `resolveType`'s cache,
and `list` (2.4%). Re-profile before going further -- the shares above were measured before any of
this landed, and three of the five entries have now moved.

### Dicts: it is the map, not the call volume

13.89 -> 12.60 KB/iteration (-9.3%) by making `dictAddEntry` return a struct tuple, take its key and
value untupled, and replacing the fold-with-a-lambda in `Dict.fromListOverwritingDuplicates` with a
top-level recursion.

**The "dicts is volume" framing this item used to carry was wrong as a statement about allocation.**
Profiled amplified, the workload is `MapTreeNode` 45.0%, `FSharpList<Dval>` 14.4%, `FSharpMap` 8.9%,
`String` 7.4% -- and closures plus state machines total **0.41%**. The ~237 package calls per
iteration cost instructions and frame pushes, not bytes, because a warm package call allocates
nothing. What is left is the immutable map itself: see the value-representation item.

### Unbox scalar Dvals

Small integers and both booleans are interned, so `1 + 1` allocates nothing. But `DInt` is 56 bytes
because `DarkInt` is a struct DU carrying space for a `bigint`, so any integer outside -128..1023
costs that. Splitting the representation, or making `Infinite` hold a reference, is the real fix.
*(estimate: moderate win, wide blast radius)*

### Dicts and records both use `Map<string, Dval>`

A cheaper small-map representation would help dicts *and* records, since `DRecord` holds its fields
the same way. Big change, no design yet. *(estimate)*

### Moving the rest of the codebase off Ply -- measured neutral, do not redo for performance

~400 `uply` sites remain outside the interpreter loop, and it is tempting to read the interpreter's
wins as an argument for converting them. It is not.

**It has been done, on the `ply-to-task` branch: 42 commits, complete, tests green, unmerged.** Its
own measurement says the hot-path swap is *within GC noise*, because `task { }` and `uply { }` are
both struct-state-machine builders. That branch was pursued for NativeAOT trimming rather than
allocation, and in its snapshot the release binary grew 381 KB. It also had to `--nowarn:3511` where
the resumable-code analyzer could not statically reduce a recursive `task`, falling back to
dynamic-dispatch state machines.

The lesson generalises: **the win is never entering a builder, not entering a different one.** That
is why the synchronous fast paths pay and why the roughly thirteen `*Sync` twins across
`TypeChecker.fs` and `Interpreter.fs` cannot be deleted by changing builder. They are the mechanism,
not a workaround for Ply.

If Ply removal is revisited, do it for trimming, binary size or dependency reduction, and measure
those. Do not expect allocation to move.

### Pool the builtin argument buffers further

Already done per-frame. An unsafe whole-VM probe suggested a little more, but the per-frame version
got essentially all of it. Low priority; recorded so nobody re-probes it.

### HTTP routing re-parses a constant -- left for a human

`parseRouteSegments` re-parses a never-changing route pattern on every request, ~32 package calls
per handler tested. Fixing it needs public stdlib API (a `makeRouter`, or a parsed field on
`Handler`), so it is a design decision rather than an optimisation.

### Compile-time type checking -- owned elsewhere

Ablation put the prize at -20% allocation and -14% wall, measured before much of the same work was
cached and synchronised, so re-measure before committing. Someone else is working on this; do not
start it here.

---

### Every remaining workload is now its own data structure

All six profiled amplified, after ten passes. Closures and state machines are under 2% in every one.
What is left is what the values *are*:

| workload | dominant | share |
|---|---|---|
| `lists` | `FSharpList<Dval>` cons cells | 88% |
| `dicts` | `MapTreeNode` | 45% |
| `strings` | `System.String` + lists of `Dval` | 36% + 26% |
| `records` | the `Map<string, Dval>` family | ~34% |

The last cross-cutting item was `ValueType.merge` matching its two arguments as a pair, which
allocated one per list element, dict entry and record field. Nested matches instead: every workload
improved, containers -3.1%, the rest 1-2%.

There is nothing else in this class left to find. `checkAndExtractLetPattern` was the last suspected
`match a, b with` and it does not appear in any profile, so changing it would be on faith. Continuing
means changing the representation.

## Larger, not yet scoped

**Value representation.** Struct `Dval` with tag and payload, cached singletons for small
ints/bools/unit, array-backed records with a shared shape descriptor instead of a `Map` per record.
Weeks, highest ceiling, highest risk, and the main remaining allocation play.

**Concurrency in the language.** Not a runtime-serialization problem: the server already uses seven
cores. What is missing is a way to *express* parallel work, and ideally to find it automatically in
sequential code. The safety predicate already exists -- `CapabilityAnalysis` folds transitive
effective capabilities and `noCaps` means pure -- so "is this lambda safe to run out of order" is
answerable per call site. A narrow first version would be `List.map`/`filter` where the lambda is
`noCaps` and the list is long enough to pay for scheduling. Two cautions: it will not help
allocation, and per-element work must exceed task overhead by a good margin. Not a priority.

---

## Correctness, not performance

**Two declarations can share a hash, and one silently wins.** A function whose signature says `TA`
will accept a `TB` and run: not an error path, a wrong-code path. `NameResolutionError` carries no
name and the canonical writer skips `originalName`, so every unresolved reference serialises to the
same two bytes; two declarations differing only in one hash identically; and `withExtras` keys by
hash with `Map.ofList`, keeping the last. Scripts hit it constantly, because `Cli.fs` hashes a
script's fns before its own types are in scope.

Not fixed here. The obvious three-line fix -- write `originalName` into the hash when unresolved --
breaks the content-addressing invariant that names never affect hashes, and was reverted. The real
fix is to stop content-hashing before resolution, using the location-derived placeholders
`LibParser/Package.fs` already has. Context, failure and options:
`notes/hashing-unresolved-refs-collision-2026-08-19.md`.


**A type argument can be violated silently.**

    type Box<'a> = { v: 'a; tag: String }
    let b = Box<Int> { v = 1; tag = "t" }
    { b with v = "str" }        // succeeds, giving Box<Int> { v: "str" }

The check that should catch it is dead. In four places in `TypeChecker.fs` the code matches `vt`,
binds the catch-all as `known`, and then calls `ValueType.merge known vt` -- merging a value with
itself, which always succeeds. Three of those predate this campaign; the fourth is
`updateTypeArgsSync`, which reproduced the idiom when extracting the synchronous twin. Letting
parameterised types onto the fast path did not widen the exposure, since the async path ran the same
self-merge, but the fix is now a four-site change.

---

## Smaller, known, unowned

- `run-in-docker` hangs after the command finishes (`cat <&0` waits for EOF); pass `< /dev/null`.
- `Builtin.debug` has no package wrapper anywhere, and its only mention under `packages/` is a
  commented-out line, so its reference count is propped up by scripts outside `packages/`.
- Re-enable the `CliTraces` suite once its network call is stubbed.
- Decide what `dark version` should do when the network is slow or absent; it costs ~700 ms.
- Unify `callBuiltinResolved` and `callPackageResolved`: same five steps, different types. Needs
  `BuiltInParam` and `PackageFn.Parameter` to share an interface.
- Make builtins opt-in, as installable extensions. Would cut startup and the builtins table.
- Mine the telemetry corpus: every checkout's `rundir/logs/telemetry.jsonl` has thousands of real
  runs, which could retarget the campaign against what people actually do.
- Runtime call stacks name package functions by content hash, which makes every error harder to act
  on. Not a performance problem, but it is in the same code.

---

## Closed, so nobody re-opens them

- **Records and enums**, **enum construction**, and **the dead gate in both record paths**: done.
  The remainder is spread thin.
- **JSON.** `Json.parse` is a third of what the call costs; nothing inside dominates. A list element
  is ~900 B, a record field ~100.
- **Making stdlib wrappers free.** A warm package call allocates *nothing*: 0, 1, 2, 4 and 8 extra
  forwarding hops all allocate 231 bytes while package calls rise 4 to 12. Only the first call to a
  given fn pays. There was nothing to win.
- **Calling a polymorphic builtin.** Claimed 7,470 B from a residual across two probe scripts;
  actually 192 B. Retracted.
- **A 2x between two record types.** Was the hash collision described below: the two rows were not
  measuring the types they named.
- **`scripts/perf/http` saturating on its own client.** Fixed with `-p` processes.
- **Sizing the binary readers' collections exactly** in `Serializers/Common.fs`. Measured 3% *worse*
  (17,486 to 18,023 bytes per package load) and reverted.
- **Inline caching at call sites.** Memoizing container `ValueType`s took those stages to 0 B/run.
- **Argument lists.** Builtins take an array, reused per frame.
- **Moving the gate to the `build-cli` job.** Both are Release builds of the same code and the
  difference is packaging, so there is no measured reason to expect different allocation. Revisit if
  the release script starts passing different publish flags.
