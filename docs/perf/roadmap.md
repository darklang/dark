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
    ./scripts/run-cli run scripts/perf/workloads/optime.dark      time per operation (costs.dark's time twin)
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

## Round 5: CLI rendering and HTTP request handling

A keypress went **283 -> 50 ms**, a view build 24 -> 12 (published), routing a request 600 -> 330 us,
and a request's allocation 76 -> 47 KB. Both gate budgets came down, 7.6% Debug and 10.4% published.

Two passes. Cutting work out of the CLI reached 141 ms and then hit a floor: 15-35 us per list
element, 5.1 us per package call. The CLI was not slow because its renderers were badly written; it
was slow because it ran many small list operations against a slow interpreter. The second pass went
there, which is why HTTP improved without being touched.

What landed: seven CLI fixes that delete work; thirteen list operations that were Dark recursions made
native; an operator fast path where the interpreter answers 20 common operations itself; elision of
package fns that bare-forward to a builtin (84% of a view's package calls); `String.slice` and the
text-measurement functions rewritten to stop building a grapheme cluster per character; and one
security fix, where a process-wide cache held a per-configuration builtin so one execution state's
SSRF-disabled config could reach another's callers.

### Closed with a size, so nobody re-attempts them

| | worth | why not |
|---|---|---|
| lambda frame elision | 7.3% of a view | needs register remapping, and only 16% of applications have a body making no call of its own |
| type checking on calls | ~0.1 of a call | ablated both argument and return checks; the cost is not there |
| `Canvas.compose` bucketing | 0.8 ms | gap-padding forces a recursive walk, and a Dark self-recursive call is ~3.2 us an element |
| opcodes for common ops at `PT2RT` | ~0.7 ms | a new `Instruction` case across six sites including binary serialisation |
| vectorizing `styledWidth`'s ASCII runs | **-9%** | runs are short and interleaved with escapes, so span setup costs more than the branches saved |
| reference-keying the `Hash` caches | nothing | a 64-char hex dictionary key looks expensive and is not |
| ASCII pre-check in `String.Normalize` | nothing | .NET already fast-paths it |
| avoiding the `bigint` round-trip | nothing | `BigInteger` is a struct and does not heap-allocate small values |

### Which builtins the round kept, and why

The line drawn, once a builtin had been measured to help: it stays if it replaces per-element work in
Dark, or if it is a primitive predicate the interpreter can answer without allocating. The thirteen
list operations are the first kind; `List.isEmpty` and `String.isEmpty` the second.

Four were removed again for failing that test. `String.endsWith` and `Dict.isEmpty` measured as doing
nothing. `Int.max` and `Int.min` did help, marginally, but they are a single comparison rather than a
loop, so they are back to `if greaterThan a b then a else b` in Dark; the Dark version allocates
slightly less, because it returns its operand rather than re-wrapping it, and routing does not move.
The two remaining marginal ones, `List.zipShortest` and `String.dropFirst`, are worth 9.2% of routing
between them and both walk their input, so they stay.

A thin wrapper called with **explicit type arguments** is never elided, because the guard requires no
type args. It costs ~3.4 us against 0.17 for an elided one. Extending it means resolving type args in
the elision path, and the parameterised return check happens either way, so not all of it is
recoverable. Narrow reach: no CLI call site passes explicit type args, and HTTP has one per request.

### Where a view's time goes now

~2,951 package calls, ~729 slow-path builtin calls, ~1,594 lambda applications and ~4,800 operators on
the fast path. All four per-call costs are measured and worked; what is left inside them is a few
percent behind a structural change. `composeRow` plus `renderSegments` is the largest single item,
about 40% of a view, and making them native is the "F# owns measurement, Dark owns layout" judgement
call rather than a perf question.


## Open, roughly ranked

### Redraw the whole frame on every keypress -- the largest item anywhere

An arrow key changes **2 rows of 40**, measured. The CLI rebuilds all forty to produce two, which is
worth most of the 12 ms a view costs -- an order of magnitude more than anything left in the
interpreter. Presenting is already cheap, so a diff at the output end is not the answer; it wants the
view built from regions that know when they are stale. The map such a change needs is what each
region costs and which ones a keypress touches; probe for that against the view as it stands then,
rather than trusting one written against this round's.

An architecture question for the CLI, not an optimisation.

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

**An interpreter round aimed at latency, not allocation.** Stachu's suggestion, and this round has
been quietly building the case for it. Rounds 1-4 optimised the interpreter for *allocation* and, by
their own account, largely exhausted the easy classes. Round 5 has now found the same thing four
times over in the CLI: there is no pathology, only volume. Measured here, per call: a 13-element
tuple literal 465 us, `List.range 1 13` 175 us, `List.filter` over 13 Ints 215 us, `List.member` over
13 Ints 70 us, a 29-field record update 11 us. That is roughly **15-35 us per list element**, and it
is the multiplier on every structural win in this round.

Which means the ceiling on interactive latency is not the CLI's code. Every fix so far has been
"do fewer list operations", and each one is bounded by how few are actually needed. Halving the
per-element cost would take a keypress from 163 ms to somewhere near 80 without touching a single
renderer.

Worth stating clearly because it is a *different* target from rounds 1-4: those measured bytes, and
the roadmap says plainly that allocation campaigns move allocation while time follows far less. This
would be a wall-clock campaign against the per-operation cost of the loop -- dispatch, list
construction, record field access -- and it needs an instrument that measures time per operation
rather than bytes per workload. `scripts/perf/workloads/costs.dark` is the closest thing we have and
it reports allocation.

**Prefetch package items while something else is running.** Stachu's idea, filed with a size on it.
Items are cached per process, so only the first touch of anything pays; the thought is to start
loading what will be needed shortly while current work runs, hiding the latency rather than reducing
the cost. It pairs with the parked "Package deserialization" item below, which is about the cost
itself and is parked for want of an instrument.

Measured, first render of each view against its second (the gap is package loading):

| view | cold | warm | penalty |
|---|---|---|---|
| Home | 162 ms | 112 ms | 50 ms |
| 1 | 149 | 113 | 36 |
| 5 | 87 | 62 | 25 |
| 2, 3, 4, 6, 7, 8 | | | 0-4 |

So the prize is real but bounded and front-loaded: the first view opened pays ~50 ms, the next
distinct one ~36, and by the fourth it is noise, because earlier views have already warmed what they
share. Views were rendered in order here, so this is the shape of a session that visits several
views, not of one that sits in a single view.

That makes it a **startup and first-visit** win of maybe 60-90 ms total across a session, against the
120 ms per keypress this round has already removed. Worth doing, not worth doing first.

Where it would live: the loader is F#, so warming a cache on a background task needs nothing from the
language. That matters, because expressing parallel work *in Dark* does not exist yet -- see
"Concurrency in the language" below. Prefetching from F# sidesteps that entirely. What to prefetch is
the open question; sidebar navigation is predictable enough (the adjacent views) that a guess would
often be right, and a wrong guess costs only work nobody waited for.

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
- **Making stdlib wrappers free -- reopened and done in round 6.** The original close was right about
  allocation and wrong to stop there: a warm package call allocates nothing, but a forwarder cost
  2.5 us of frame in *time*. Eliding it took a view build 76 -> 66 ms. Allocation-only conclusions
  should not close time questions.
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
