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

## Round 5 in one line, before the detail

283 to 141 ms a keypress across seven fixes, and every one of them removed work rather than making
anything faster: an invariant recomputed in a loop, constants rebuilt per call, a handler computing
outcomes for keys nobody pressed, a general helper on a hot path, a search asked an existence
question, a sort of already-sorted data. That seam is mined out -- the last sweep for more of it found
0.2 ms -- and what remains in the CLI is architectural.

Every probe bottomed out at the same constant: 15-35 us per list element, 4.6 us to read a record
field, 5.1 us to call a package fn. **The CLI is not slow because its renderers are badly written; it
is slow because it runs many small list operations against a slow interpreter.** That is round 6, and
it serves HTTP, eval, scripts and the LSP at the same time. See "An interpreter round aimed at
latency" below, which now has three measured targets rather than a hunch.

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

**Done: `let f () = <constant>` rebuilds the constant on every call; `val` evaluates it once.**
`workspaces` (9 records with nested lists) cost 910 us a call and was called ~11 times per sidebar
build; `viewSpecs` (13 tuples) cost 465 us. Neither depends on state. As `val`s they are built once.

| | before | after |
|---|---|---|
| `sidebarRows` | 18 ms | **9** |
| `handleKey` | 23 ms | **11** |
| `viewAtSize` | 120 ms | **108** |
| keypress, end to end | 198 ms | **176** |

Worth sweeping for elsewhere: any `let f () =` whose body is a literal is paying this. `extensionViews`
is the obvious next one, but its records hold function fields, and lambdas stored in a package value
have bitten us before, so it needs testing rather than assuming.

**There is no interpreter pathology here, only volume.** Measured per call: building a 13-element
tuple list 465 us (~36 us an element), `List.range 1 13` 175 us, `List.filter` over 13 Ints 215 us,
`List.member` over 13 Ints 70 us. So ~15-35 us per list element is simply what the interpreter costs,
and 2.3 ms for `visibleViews` was thirteen elements through a filter with a `member` inside it. The
wins in this area are structural -- do less list work -- not a fix to any one function.

**Done: `overlay` trimmed every placed segment even when nothing overlapped.** With the view build
now dominant, splitting it gives `renderFull` 46 ms (252 spans) and `Canvas.toView` 63 ms. Inside
`toView` the per-row floor is 1 ms for 40 empty rows and bucketing all 252 spans is 3 ms, so ~60 ms
was `composeRow`. `overlay` ran `List.map` + `List.flatten` + `List.append` per span; stubbing it to
a plain append probed the ceiling at 63 -> 43 ms. A no-overlap fast path (one `List.any` instead of
three walks and a list allocation per segment) took it to 53.

| | before | after |
|---|---|---|
| `Canvas.toView` | 63 ms | **52** |
| `viewAtSize` | 106 ms | **98** |
| keypress, end to end | 176 ms | **163** |

Half the probed ceiling. The rest needs the `any` walk gone too, which means knowing a row is built
left to right rather than rediscovering it per span.

**Negative result: bucketing spans by row is not worth touching.** `List.append existing [span]`
looks quadratic, but a row holds ~6 spans, so it walks six elements. Changed to `List.push` with a
`reverse` per row: 62 -> 63 ms, flat, reverted.

**The per-element cost is `List.fold`, and it is a Dark recursion.** Round 5's every finding
bottomed out at "~15-35 us per list element" with no way to say which operations those microseconds
were in. `scripts/perf/workloads/optime.dark` is the time twin of `costs.dark` and now says. Above
the harness baseline (21.3 us, stable to 250 ns across the run):

| operation | above baseline |
|---|---|
| `List.map` over 5 | **+104 us** |
| `List.filter` over 5 | **+93 us** |
| `List.range 1..5` | **+76 us** |
| `List.fold` over 5 | +48 us |
| build a 5-element list literal | +6.4 us |
| `Int.toString` | +8.9 us |
| call a 1-arg package fn | +5.1 us |
| apply a lambda | +4.1 us |
| build a 2-field record | +2.4 us |
| if/else | +3.1 us |
| match on an Int | +0.4 us |

Building a list is cheap. *Iterating* one is 10-40x anything else, and the reason is in
`stdlib/list.dark`: `fold` is a Dark recursion, so it costs a package call, a match and two lambda
applies per element; `map` and `filter` are `fold` plus a `push` builtin per element plus a
`reverse`; `range` is its own Dark recursion with a `push` per element. Five interpreter operations
an element, at a few microseconds each.

**Target 2, four cuts, measured through the native-`map` amplifier.** It builds one VM per element,
so `steady.dark` builds 10,000 and the gate reads the per-application cost magnified.

| | gate | ~total |
|---|---|---|
| native map, untouched | 843.7% over | 73.6 MB |
| + pool the VM (`VMState.reuseFor`) | 257.0% | ~27.9 MB |
| + cache the `Apply` instruction per arity, write registers directly | 188.7% | ~22.5 MB |
| + reuse the root frame, its registers, and the `InstrData` array | 144.7% | ~19.1 MB |
| + stop clearing `framePool` on reuse | 94.6% | 15.2 MB |
| + `executeApplicable` returns `Ply`, answered synchronously | 60.0% | 12.5 MB |
| + the builtin's own per-element loop asked synchronously too | 53.4% | 12.0 MB |
| + keep the root frame's id instead of a fresh `Guid.NewGuid()` | 53.3% | 12.0 MB |
| + a thread-static slot instead of a `ConcurrentBag` | **46.9%** | **11.4 MB** |

And on the clock, which is what round 6 is actually for -- `map over 5`, above baseline: Dark **+99
us**, native map before target 2 **+55**, native map now **+40**.

The instruction cut: the per-call `LoadVal` instructions existed only to move the callable and its
arguments into registers, so they are gone and the caller writes those registers itself. What remains
is a single `Apply`, identical for every application of the same arity, so its `InstrData` is built
once ever and shared.

**The last cut was a bug in the first.** `framePool` holds returned `CallFrame`s keyed by register
count for the next push to reuse -- the mechanism that makes a lambda's frame cheap. `reuseFor` was
clearing it on every application, so each one allocated its lambda's frame afresh and threw it away.
Keeping it is safe: the root frame is removed rather than returned when it completes, so it is never
in the pool and there is no double ownership.

**Negative result: the `Ply` wrapper around the interpreter loop is not the cost.**
`Interpreter.execute` is `uply { return! executeInnerTask }`, so a caller already inside a `task`
awaits a Ply wrapping a Task, a builder each way per application. Added a Task-returning entry point
and used it: 15.2 MB either way. Reverted.

**The `Ply` cut is the interesting one.** `executeApplicable` returned `Task<ExecutionResult>` and
built a `task` state machine per application. It returns `Ply` now and asks `Ply.trySync` first, so a
lambda that does not await -- an arithmetic body, a field read, a comparison, which is nearly all of
them -- costs no builder at all. Same shape as the `Ply.trySync` fast paths rounds 1-4 put through the
type checker. Two callers needed `|> Ply.toTask`: `HttpServer.executeHandler` and one test.

Applying the same question inside the builtin's own loop -- `trySync` per element rather than a `let!`
that allocates a continuation to await something already finished -- was worth another 0.5 MB.

**The frame id was the same trap the interpreter already documents.** `nextFrameId` derives a frame
id from a counter, and says why: "`Guid.NewGuid()` draws from the cryptographic RNG, and a frame push
happens tens of thousands of times in a single command." `reuseFor` was calling `Guid.NewGuid()` once
per lambda application. It keeps the frame's existing id now -- the frame is being reused, and frame
identity is internal to a VM. A `Guid` is a struct, so this is invisible to the gate and shows on the
clock: `map over 5` 60,500 -> 57,750 ns.

**The pool was allocating to hold the pool.** `ConcurrentBag.Add` allocates a node per add, once per
application. A VM's interpreter loop is single-threaded, so a thread-static slot needs no
synchronisation and no node, and one slot suffices: a nested application finds it empty and builds
its own. Worth 12.0 -> 11.4 MB, and `ExecutionState.applicableVMPool` is gone with it.

**A stdlib wrapper costs more in time than the builtin it wraps.** Same builtin reached both ways,
above a ~20.7 us baseline, repeated three times:

| | above baseline |
|---|---|
| `Builtin.stringLength "hello"` | **+1.75 us** |
| `Stdlib.String.length "hello"` | **+4.0 us** |

So the package-call machinery -- resolving the fn, type-checking the arguments, pushing a frame,
checking the return type -- is **+2.25 us**, more than the call it wraps costs in total.

**This qualifies a closed item, for the second time in this round.** "Making stdlib wrappers free"
was closed on the finding that a warm package call allocates *nothing*: 0, 1, 2, 4 and 8 forwarding
hops all allocated 231 bytes, so "there was nothing to win". That is true and was measured. In time
there is a great deal to win, and the two facts are not in tension -- they are answers to different
questions, and the closed item only ever answered one of them.

**Which suggests the shape of the fix: elide the wrapper.** A stdlib fn whose body is exactly
`Builtin.g a b` carries no information a caller needs at runtime. Detecting that at instruction
generation and emitting the builtin call directly at the call site would take 2.25 us off every such
call. The cost is in call stacks and tracing, which would stop naming the wrapper -- and the roadmap
already records that "runtime call stacks name package functions by content hash", so that surface is
due attention anyway.

**Sized: 44% of a view build's package calls are thin wrappers.** Ran a workbench view with
`detailedTiming` on, resolved the hashes against `locations`, and checked each function's body in
`packages/`. 4,197 of 9,505 calls, at 2.25 us each, is **~9.4 ms off a 75 ms view build -- 12.5%** --
and it would apply to every workload, not just this one.

The top of the list, by calls in one view build: `List.push` 771, `Int.greaterThan` 590,
`List.append` 520, `Int.toString` 303, `Dict.get` 292, `Text.styledWidth` 289, `Bool.not` 283,
`String.repeat` 269, `Dict.setOverridingDuplicates` 252. The 56% that are not wrappers are led by
`List.fold` at 1,671, `List.any` 1,007 and `List.findFirst` 668 -- the Dark recursions target 1 is
about, which is a neat confirmation that the two targets are disjoint and additive.

**Two ways to elide, and the callee-side one is far more contained.** Doing it at the call site means
the caller's instruction generation must know the callee's body, which adds a load dependency.
Doing it in the Apply path -- a cached "this fn is a thin wrapper for builtin B" flag, checked before
pushing a frame -- changes no instruction generation at all.

**Ablated the type check, and it is not where the time is.** Stubbed `checkPkgParamsSync` to accept
everything and re-measured: the wrapper's overhead over the raw builtin goes **2.25 -> 1.75 us**. So
argument type-checking is ~0.5 us of it, 22%, and the other 1.75 us is the TST shadow, building the
argument sequence, **the frame push**, and the return type check.

**Which largely dissolves the semantics worry.** An elision that keeps the wrapper's argument and
return checks and skips only the frame push would still take most of the 2.25 us, and
`Stdlib.String.length 5` would report exactly what it reports today. That is a much smaller thing to
be sure of than "skip the checks and accept different errors".

**Done, and it is round 6's first real win.** `thinWrapperOf` in `Interpreter.fs` recognises the
two-instruction forwarder and runs the builtin in the caller's frame. It qualifies only when the
wrapper's signature is *identical* to the builtin's, which is what makes the checks question moot:
the builtin then checks the same arguments against the same types and produces the same return type,
so `Stdlib.String.length 5` reports what it always did. Calls with explicit type args and partial
applications keep their frame. Cached by hash.

Same binary either side, toggled by an env var: `viewAtSize` **76 -> 66 ms**, ten workbench views
mean **70 -> 61**, `steady.dark` **353 -> 286 ms** with frame pushes **61,405 -> 41,004**, and the
wrapper's overhead over its builtin **2.5 -> 0.5 us**. On `optime.dark`, each figure minus its own
run's baseline: `cons` -53%, `range` -25%, `map` -22%, `filter` -16%. `fold` and `member` did not
move, being Dark-side loops rather than forwarders. Allocation unchanged -- frames were already
pooled, so this is time, not bytes.

**Two bad measurements on the way, both the playbook's lesson.** The first cut came in 24% over the
allocation budget with the elision provably never firing: `match cache.TryGetValue k with | true, v`
allocates the tuple, which F# only sometimes optimises into the out-param form. Explicit byref, and
it is free. 45 bytes on a per-package-call path is 1.8 MB. Then I decided the frame's single argument
buffer was thrashing, made it one per arity, and measured no difference -- while the type-param guard
was still blocking every wrapper, so nothing was being elided. Re-measured with elision actually on,
it is within the gate's rounding either way. Kept, because the reasoning survives the null result,
but this workload does not show it.

**The commit message for this says "package calls 41,404 -> 21,004"; read it as frame pushes.**
`packageCallCount` was incremented at frame push, so eliding a call stopped counting it. The counter
now counts elided calls too and `framePushCount` does not, which makes the gap between the two the
thing elision actually buys.

**Left open here.** The elided wrapper no longer appears in the detailed-timing profile or in a
runtime call stack, since there is no frame to name it. Nothing depends on that today and the whole
suite is green, but an error raised inside an elided builtin names the builtin rather than the
wrapper the user called.

**Round 6's second win: let-pattern binding.** Found by re-profiling the view build after the
elision landed, which is the argument for re-profiling after every win.
`CheckLetPatternAndExtractVars` was allocating **131 bytes per `let`**, 700 KB per view, second only
to `Apply`. The plain `let x = v` case was already free, so all of it was destructuring going through
`checkAndExtractLetPattern`, which returns `bool * List<Register * Dval>` -- a tuple and a cons per
bound variable, plus a closure for the `List.iter` that assigns them.

`assignLetPattern` writes registers as it walks. A match pattern has to collect first, because a
failed alternative must leave the frame untouched for the next case; a *let* pattern that fails is
fatal and the frame raises, so nothing reads the partial writes. That asymmetry is the win. Both
callers moved, the opcode and lambda parameter binding.

`CheckLetPatternAndExtractVars` **700 -> 50 KB/view**, all opcodes **3,967 -> 3,288 KB/view**, the
view build 66 -> 65 ms, gate 7.9 -> 7.8 MB.

**Allocation and time have come apart in this round.** A 17% cut in a view build's garbage bought
about 1.5% of its wall clock. Frames are pooled and gen0 is cheap, so this is expected rather than
disappointing -- but the campaign's framing (decide with allocation, since it repeats) needs the
qualification that a latency target now wants both numbers, and a win should be claimed on whichever
it actually moves. `gate` is still right to assert allocation only; it is a regression alarm, not a
latency instrument.

**`scripts/perf/workloads/viewprofile.dark`** is the instrument for this: one view build with
detailed timing on, dumping per-opcode and per-stage allocation. It is what both of this round's wins
were found with.

**RETRACTED: "target 1 is inverted".** I claimed a Dark-side `map` beat the native builtin by 25%
and closed target 1 on it. `Stdlib.List.map` is not a builtin. It is
`fold list [] (fun acc elem -> push acc (fn elem)) |> reverse`, written in Dark, and so are `fold`,
`filter` and `range`; only `push`, `reverse` and `length` are builtin wrappers. Both sides of that
comparison were interpreted, and the one I labelled "native" simply does more work per element -- two
lambda applications, a push and a reverse against one application and a push. Target 1 stands exactly
as it was. The check I skipped was one `view` of the function I was calling native.

What the amplifier does measure, correctly, is the cost of that extra work:

    map 50 by direct Dark recursion            9.3 us/element
    Stdlib.List.map (fold + lambda + reverse) 12.6 us/element

**And the allocation figure with it.** "26 bytes per application difference" was the same two
interpreted paths, so it says nothing about a builtin applying a lambda either.

**No List operation applies a lambda from F# today.** `executeApplicable` has exactly two callers in
the builtins, `Stream` and `HttpServer`. So target 2 is not a cost the CLI is paying now: it is the
ceiling on target 1, which is what makes the order in the campaign brief right. Land the native list
operations first, and target 2 becomes the thing standing between 2x and 3-4x.

**Targets 1 and 2 are done, and they only worked together.**

Native `fold`, and nothing else. It is the one list operation with no typing question: the blocker in
the brief -- that a native `map` must compute its result ValueType the way `listFlatten` does -- is
about *building a list*. A fold returns its accumulator, which carries its own type, and `map` goes on
building with `push` as before. `map`, `filter` and most of the module are written on `fold`, so they
all move.

Above each run's own baseline, on `optime.dark`:

    fold over 5     53.0 -> 25.9 us    2.05x
    filter over 5   74.8 -> 49.4 us    1.51x
    map over 5      78.5 -> 53.6 us    1.46x

    viewAtSize      65 -> 54 ms
    steady.dark    283 -> 221 ms, 7.8 -> 7.7 MB
    suite: lists 276 -> 221 ms, dicts 130 -> 110, nothing regressed

**Target 2 was the bill for it, exactly as the brief said.** A native fold applies the lambda from F#,
and on the old path that put the gate **58% over budget** -- the same trap as the last attempt at a
native list operation. Five allocations per application, largest first: the interpreter loop is a
`task` and a synchronously-completing `task` still allocates its `Task`; `execute` wrapped that in
`uply { return! ... }`; `applyInstrsFor` built a `System.Func` per call for a factory that runs once
per arity ever; `executeApplicable`'s three local functions all capture `vm` and so were all allocated
on every application; `VMState.reuseFor` set `rootInstrData` to a reference tuple. 58.1% over, then
37.2, 19.5, 8.4, then 7.7 MB against a 7.8 budget.

`executeSync` is the shape worth remembering: the same loop with no builder, running until something
actually awaits and handing over when it does, sharing `runFrame` and `returnFromFrame` with the task
loop so the frame-pop bookkeeping has one copy. Same trick as `Ply.trySync`, one level up.

**On its own, target 2 measures almost nothing** -- gate unchanged, `fold over 5` 53 -> 47.6 us. No
list operation applied a lambda from F# before this, so its whole value was as the enabler. Worth
remembering before someone benchmarks a prerequisite in isolation and concludes it was not worth it.

**Fixing `alloc-profile` is what made this tractable.** Every step came off that profile, and it was
sharp, not flat: one item at 50%, the next at 35%, and it stayed sharp for five rounds. Its header
says a flat profile means stop profiling and build a comparison; the corollary is that a sharp one
means keep going.

**Still open here.** `range` is Dark and applies no lambda, so it is a different change with a
different argument. And the `Task` is gone only from the synchronous path -- anything that genuinely
awaits still allocates one per entry.

**Target 1 is finished: `map`, `filter` and `range` are native too.** `map` and `filter` were written
on `fold`, so each element cost *two* lambda applications -- the fold's own
`fun acc elem -> push acc (fn elem)` and the caller's `fn` inside it -- plus a push and a final
reverse. `range 1 50` was fifty recursive package calls and fifty pushes, and the reference workload
builds one per iteration, so `range` was most of that workload without anyone having said so.

Absolute, on `optime.dark`, across the round:

    map over 5      97.5 -> 33.0 us
    filter over 5   93.0 -> 31.8 us
    fold over 5     71.3 -> 30.5 us
    range 1..5     102.8 ->  9.25 us

    viewAtSize     76 -> 50 ms
    steady.dark   353 -> 54 ms, instructions 115,643 -> 4,443 after range alone
    suite: lists 276 -> 55 ms, dicts 130 -> 55, strings 63 -> 25, records 25 -> 20

**Read the absolute numbers, not "above baseline" ones.** `optime`'s baseline row is a fold over a
range, so it fell from 18.3 to 5.1 us over this round. A subtracted figure taken now is not comparable
with one taken at the start.

**The typing was the work, exactly as the brief said.** `map`'s result element type comes from the
values the lambda returned: it merges them the way `listFlatten` does and hands what will not merge to
`DvalCreator.list`, which is what makes `map [1;2] (fun x -> if x == 1 then 1 else "two")` a type
error rather than a badly-typed list. `filter` returns a subset of what it was given, so it keeps the
input's ValueType and has nothing to merge. An empty `range` keeps an unknown element type, because
the Dark version returned the literal `[]` and only `push` ever made it a `List<Int>`.

**`filter`'s predicate error is `Bools.ConditionRequiresBool`, not `IfConditionNotBool`.** The Dark
body was `if f elem then`, so that is the error it raised. The two read almost identically
("Encountered a condition that must be a Bool" against "If condition must be a Bool") and are
different cases. Three testfiles assert the exact text, one of them a Dict test, since `Dict.filter`
routes through this.

**`range` in `bigint` costs 30% of the workload in boxed `BigInteger`.** Which is the obvious way to
write it, since `DInt` holds a `DarkInt`. It put the gate 22.5% over budget. On `int64` where both
ends are `Finite` it is free, and the wide path is kept for the ranges that would not fit in memory
anyway.

**Budget: 7.8 -> 7.2 -> 7.3 MB.** The last move is upward, 1.4% more allocation for 3x less time on
the same workload. Stated rather than absorbed.

**`suite`'s `recursion` alloc/iter column is noise.** It read 0.04, 0.76, 0.28, 0.28 KB across four
runs of unchanged code. That workload is self-recursion and Int arithmetic with no containers, so it
allocates near nothing in total and the per-iteration figure is a small difference of large numbers.
Its time, flat at ~1120 ms, is its signal.

**A full instrument sweep, because most of round 6 was measured on four of them.** Everything below
was unreported until this pass.

    keystroke, end to end        141 -> 103 ms median (n=12, 96-126)
    workbench.dark, per view      61 -> 45 ms mean over ten views
    suite: dicts                 130 -> 55 ms
    suite: strings                63 -> 24 ms
    suite: lists                 276 -> 57 ms
    suite: records                25 -> 21 ms
    HTTP, alloc per request    76.31 -> 62.90 KB, -17.6%, throughput unchanged at ~5,000 req/s
    crosslang, allocation      11.84 -> 9.90 KB/iter, 7.0x -> 5.8x node
    crosslang, time             ~43x -> ~9x node (node 6.35 ms, python 8.58, Dark 57)

The HTTP number is the one nobody would have looked for: an interpreter round aimed at CLI latency
took 17.6% off what a request allocates, without touching the server. Throughput did not move, which
the roadmap already predicted -- allocation is not what limits it here.

**`scripts/perf/bench` had never been run in this clone** -- no fixture, so every scenario exited
without measuring. There is one now, and a baseline in `rundir/perf/timeseries.jsonl`. It cannot say
anything about this round, only about the next one.

**`checks` caught a real regression that the 10,342 tests did not.** Making the list operations native
dropped the lambda's frames from the call stack of any error raised inside one. Fixed; see the commit.
Worth knowing that this is what `checks` is for, and that it is by-hand, so a round that never runs it
can ship this.

**The gate drifts within a session.** It read 7.2, then 7.3, then 7.4 MB across this pass with the
budget moving under it. The store grows with every package reload and this round did many; the
roadmap already says a debug gate comparison taken across a work session is not a comparison. Read it
against a freshly restored store, or read `suite`.

**Rank package functions by call count, not by the profiler's timing.** The per-function timing in
`InterpreterStats` is *inclusive*, so under recursion it reports whichever function is outermost and
tells you nothing. Call count works, because a Dark recursion counts once per element. One workbench
view, before this pass:

    List.any            1,007
    List.findFirst        668
    Int.max               590
    Dict.get              292
    Option.withDefault    275
    Canvas.overlay        252

The hashes come back unnamed -- `run-cli view` will not take a bare hash -- so resolving them is a
query against `locations` in `rundir/data.db`. `scripts/perf/workloads/viewprofile.dark` produces the
data.

**`List.any` and `List.findFirst` are native**, both per-element recursions with a lambda, the same
shape `fold`/`map`/`filter` had, both short-circuiting. `member` and `findFirstIndex` are written on
`findFirst` and got it for free. **`Int.max` and `Int.min` are builtins**, having been a package frame
wrapping a builtin call wrapping an if.

    viewAtSize              50 -> 46 ms
    member of a 5-list      46.0 -> 34.3 us
    package calls per view  6,953 -> 5,657
    instructions per view   67,053 -> 54,997

**`Int.max` was worth about 1 ms, which is what 590 calls at ~2 us of frame predicts.** The prediction
matching is the useful part: the ranking now sizes one of these before it is done.

**The profile is flat now, and that is the finding.** The top remaining item is 590 calls out of
5,657, and what is left is a long tail of small functions each paying a full frame -- `Dict.get`,
`Option.withDefault`, `Option.isSome`, `Tuple2.second`. Picking them off is worth roughly a
millisecond each, and there is no longer one that dominates.

**Correction to the line that was here: 5,657 package calls is not 5,657 frames.** `packageFnTiming`
records 1,762 of them per view; the other ~3,895 are thin forwarders, elided, with no frame at all.
And a frame-pushing call is ~5 us, not the ~2 I wrote. The corrected budget for a 46 ms view:

    lambda applications      ~3,660 x 3.7 us   13.5 ms
    package calls with frames 1,762 x 5.1 us    9.0 ms
    elided forwarders        ~3,895 x 1.25 us   4.9 ms
    builtin calls, direct    ~7,000 x 1.0 us    7.0 ms

**Target 2 is finished, and this is the measurement that says so.** The brief has it capping target 1
at 2x, because `executeApplicable` booted a VM per application. After the `Task`, closure, struct-tuple
and one/two-argument work, it no longer does anything the interpreter's own Apply does not:

    20 applications of one lambda, by a builtin       3.7-3.9 us each
    20 applications of the same lambda, by Dark       3.3-3.6 us each

Measured with a throwaway `listApplyN` builtin applying a lambda in an F# loop, so no Dark loop sits
in the way, minus a zero-application row for the builtin's own call cost; three runs, stable. The
Dark figure subtracts the recursion machinery a Dark loop needs (a 3-argument package call, a compare
and a subtract, ~7.0 us).

**So the reentrant-VM idea is dead, and it is worth saying why it looked alive.** Running a
builtin-applied lambda on the caller's VM instead of a borrowed one would save the VM setup -- which
is now most of nothing. Upper bound on the whole change: 0.5 us x 3,660 applications, under 2 ms of
46, against real risk (the outer `runFrame` is mid-instruction, so `currentFrameID`, `frameToPush` and
`finalResult` all need saving and restoring). Not worth it.

**A stub probe cannot be used on this path.** Stubbing `runLoaded` to skip the lambda breaks CLI
startup, since the CLI's own registry is built with `map` and `fold`; a stub returning a constant
fails the fold's type check, and one returning its argument fails elsewhere. The throwaway *builtin*
worked because it adds a path rather than breaking one.

**What is actually left is the cost of a frame**, for lambdas and package functions alike: 5,422 frame
pushes per view, and the two largest buckets above are both "what a call costs". That is one target,
not two, and it is where a round 7 would start.

**Ablation says a package call is not paying for what it looks like it is paying for.** Env-gated
stubs for the four obvious costs, each measured on `optime`'s `call a 1-arg fn`:

    none ablated                        5.25 us
    no argument type check              5.00
    no return type check                5.00
    no type-symbol-table work           5.25
    no tracing                          5.00
    all four                            4.50

Together they are **0.75 us of 5.25, 14%**. The type checks are not the cost, and neither is the TST,
which measured as nothing at all. The other 4.5 us is the frame machinery: the `Apply` dispatch, the
`ApplyContext`, the registers, the dictionary the frames live in, and the round trip through the loop.
Anyone who opens this expecting to find a type-checking hot spot should read this first.

**One piece of that was cheap and is done.** Both interpreter loops asked
`callFrames.ContainsKey vm.currentFrameID` and then indexed it -- two hashes and two probes of a
`uuid`-keyed dictionary per turn, for one frame. One `TryGetValue`: `call a 1-arg fn` 5.25 -> 4.9 us,
`apply a lambda` 2.50 -> 2.25, `viewAtSize` 46 -> 44 ms.

**Negative result: the frame dictionary is not the cost, and the round 7 target named here was
wrong.** A throwaway builtin doing one insert, one lookup and one remove on a
`Dictionary<Guid, obj>`, priced against a zero-cycle row: **50-60 ns per cycle**, three runs. Against a
package call's 4.9 us that is **1.4%**. Replacing it with a frame stack is a wide change --
`callFrames`, `pendingCallArgs`, `framePushTimestamps` and the tracer's per-frame maps are all keyed
the same way -- for six hundredths of a microsecond. Do not.

So the accounting for a frame-pushing package call now reads: ~1.25 us is the callee's own body (in
`optime` that is `i + 1`, an elided forwarder to a builtin), 0.75 us is the four ablatable costs,
0.06 us is the dictionary, and the remaining ~2.8 us is frame setup plus two turns of the interpreter
loop. Nothing in it is a hot spot; it is the shape of the loop.

**Widening the elision was worth more than any of that.** `Stdlib.Dict.get` is
`Builtin.dictGet dict key` and was not being elided, nor was any forwarder whose signature mentions
`Option`, `Result` or a user type: `TCustomType` carries the name as written beside the resolved hash,
so a builtin's `TypeReference.option varA` and the Dark `Option<'a>` wrapping it are not equal, and
`sameSignature` compared whole `TypeReference`s. `sameType` compares the resolved hash and the type
args and ignores the name.

    frame-pushing package calls per view   1,762 -> 1,292
    frame pushes per view                  5,422 -> 4,397
    viewAtSize                                46 -> 43 ms

**ANSWERED, and it was not what it looked like.** The open question here was why eliding
`Stdlib.HttpClient.request` broke 36 testfiles, including the SSRF-guard assertions
(`get "http://localhost"` must be `errInvalidHost`). It had nothing to do with effectful builtins.

The wrapper cache stored the resolved `BuiltInFn` against the package hash, reasoning that the builtin
table is fixed for the process. It is not: `Builtins.Http.Client.builtins` takes a `Configuration`, so
two execution states hold two different `BuiltInFn` values under one name -- one blocking localhost,
private ranges, cloud-metadata and non-http schemes, one blocking none of it. Whichever was built
first won for every caller after it. The cache now holds the *name*, and the builtin is resolved
against the caller's own table; the whole suite passes with no restriction on which builtins may be
elided, and the `noCaps` guard added for this is gone.

Latent since the elision landed, and unreachable until the detector learned to see through
`TCustomType`'s original name -- at which point `HttpClient.request`, a bare forwarder like any other,
became elidable.

**The diagnostic lesson is worth more than the fix.** It reproduced only in the *full* suite; the same
tests pass under a filter, because a single execution state never disagrees with itself. I read that
as "eliding effectful calls is unsafe", guarded on capabilities, and wrote it down -- which was wrong
and would have stood as received wisdom. A bug in shared process state hides from any filter narrow
enough to build only one of the things that disagree. When a failure needs the full suite to appear,
suspect state shared between tests before suspecting the feature under test.


(The paragraph that stood here proposed the frame stack as round 7's target. The probe above
retracts it.)

**Native `any`/`findFirst` cost the HTTP path 33% of its allocation, and nothing was watching.**
An HTTP request: **62.90 -> 83.90 KB** at the commit that made them native, 85.17 at the branch tip.
Bisected by checking out each commit and re-measuring; every figure repeatable to two decimals.

`gate` asserts allocation on `steady.dark`, which calls neither. `suite` has no workload that does.
`optime` and `costs` had rows for `map`, `fold` and `range` but not these. So it went unnoticed for
four commits and turned up only in a full sweep run for other reasons. Both files have rows now.

**The trade is the general shape of this work.** Making a lambda-per-element Dark recursion native
removes the recursion's frames and adds an `executeApplicable` per element, and the application
allocates more than the frame did -- 160 B against 134 B in `lambdaalloc.dark`. `map` and `filter`
came out ahead on bytes as well as time because they also removed a *second* lambda application per
element and a whole `reverse`. `any` and `findFirst` removed only the recursion, so: faster, and
heavier. Before making the next Dark recursion native, count how many applications it removes, not
just how many frames.

Kept, not reverted -- this round is aimed at latency and the CLI, `viewAtSize` is 44 ms against 46,
and the HTTP path is not what it is aimed at. But it is a decision, not an accident.

**Where round 6 stands, measured at the tip:**

    optime, above baseline
      call a 1-arg fn        4.9 us     apply a lambda       2.25 us
      call a builtin         0.75 us    via a wrapper        1.25 us

    viewAtSize                44 ms   (76 at the round's start)
    keystroke, end to end     92 ms   (141 at round 5's end)
    workbench, per view       42 ms mean over ten
    steady.dark               54 ms, 7.4 MB
    suite: lists 56 ms, dicts 63, strings 25, records 21, json 24, containers 13
    HTTP  4,400-4,500 req/s, 85.17 KB/request
    crosslang: Dark 9.90 KB/iter against node 1.72, python 0.47

**And the cost model is complete enough to say there is no hot spot left.** A frame-pushing package
call is 4.9 us: ~1.25 of it the callee's own body, 0.75 the four ablatable costs (argument and return
type checks, the type-symbol-table work, tracing), 0.06 the frame dictionary, and ~2.8 spread across
frame setup and two turns of the interpreter loop. A builtin call is 0.75 us, of which the argument
type check is 0.25 and the result check and tracing are nothing. Every one of those was measured by
ablation, and every single suspect came back smaller than it looked.

**65% of the builtin calls in a view build are arithmetic and comparison.** 7,049 of 10,881, with
`add` alone at 1,562 and the four comparisons at 3,774. Each goes Apply -> elided wrapper -> builtin
dispatch -> argument type check -> `Dval` boxing, at 1.0-1.25 us, so roughly **8 ms of a 42 ms view
build is spent adding integers through the generic call machinery**. The interpreter has no opcode
for `+`.

**Done, and it was the largest single win of the round.** The interpreter evaluates the ten `Int`
operators itself when both operands are `Int`, and takes the ordinary path for everything else.

    viewAtSize                42-44 -> 33-34 ms
    steady.dark                  55 -> 38 ms
    optime: fold over 5       30.75 -> 19.75 us,  map 33.0 -> 22.5,  any 29.5 -> 19.5
    suite: recursion           1144 -> 676 ms,  lists 56 -> 39,  records 21 -> 15

`recursion` halving is the tell: that workload is `depth` and `fib`, nothing but arithmetic and calls.

Three things to know if this is extended. It declines while tracing is on, because a builtin call is
recorded with its arguments and result when it returns and skipping that would drop every arithmetic
operation from the trace -- the same shape as eliding an effectful forwarder. It still increments
`builtinCalls`, so the stats do not start lying. And dispatch is one probe of a ten-entry table keyed
by `FQFnName.Builtin`: the probe that sized this matched on the name string and got 36 ms where the
table gets 33, so the dispatch is worth about 3 ms on its own.

**And the fast path now runs before the call machinery, not inside it.** It sat in
`callBuiltinResolved`, already past the `ApplyContext`, the builtin-table lookup and an `ArgSeq`.
`a + b` compiles to a direct builtin call, so `Apply` can take it directly:

    add two Ints        1.0 -> 0.5 us
    compare two Ints   0.85 -> 0.5-0.75
    viewAtSize        33-34 -> 31-32 ms
    steady.dark          38 -> 35 ms

`tryIntOp` stays where it was, for the same operators arriving through an elided wrapper.

**Two allocation traps on that path, both worth knowing.** `match a, b with` allocates the pair, and
this runs for every two-argument `Apply` whether or not it is an Int operator: 4.4% of the reference
workload, 10% of a view build. Nested matches fix it, and the warning already existed twenty lines
away on `biRestPs`. A `let mutable` would have been worse still -- the rest of `Apply` has `uply`
blocks, so a mutable a continuation captures becomes a heap ref cell on every instruction.

I guessed the cause twice and was wrong both times; `alloc-profile` named it in one run,
`System.Tuple<Dval, Dval>` at 8.76%. On this path, profile before theorising.

**What a compile-time opcode is still worth.** An Int operator is now 0.5 us where `match on an Int`
-- a real opcode, no `Apply` -- is 0.25. So emitting one from `PT2RT` is worth ~0.25 us x 7,049 calls,
about **1.8 ms of a 32 ms view build**, down from the 4.6 ms it was worth before this commit. It needs
a new instruction with a fallback for non-Int operands, and that is the remaining structural step.

**Conversions are not in the keypress path.** A view build calls 45 distinct builtins, and none of
them is a PT/RT/DT conversion or reflection builtin -- the only one that appears at all is
`pmOwnerHasItems`, twice. Whatever the mapping layers cost, a keypress does not pay it.

**They are, however, most of what a CLI invocation costs.** `dark eval 1L + 2L`: **611 ms total, 354
of it in `cli.execute`**, for 2,473 instructions and 270 package calls. The interpreter work in that
is microseconds; the rest is getting the code ready to run -- package blobs read from SQLite and
deserialised, names resolved. `alloc-profile` on a trivial eval is flat at 37 ticks, so it is not
allocation-bound; it is I/O and deserialisation CPU. This is the "package deserialization: real, but
unmeasurable" item further down, and `bench`'s phase split now measures the outside of it even if not
the inside. Every `dark` command pays this, and so does every build-loop iteration.

**`scripts/perf/workloads/viewalloc.dark`** is new: bytes allocated to build one workbench view, the
allocation counterpart to `keypress.dark`. It is what caught the VM-pool regression, and neither
`gate` nor `suite` would have.

**Target 3 is retired properly, with the measurement it always needed.** The brief has "record field
access at 4.6 us, which is on every path in the language". Two things are wrong with that.

The 4.6 us was an artifact, retracted earlier in this round. And `optime`'s `read a record field` row
is `preBuilt.a`, where `preBuilt` is a **`val`** -- so it measures loading a package value and *then*
reading a field, and the ~0.6 us it reports is mostly the load. The field read itself is 25 ns.

That number comes from a throwaway builtin doing 100 lookups against each representation, with a
zero-lookup row subtracting the builtin's own cost:

    F# Map.TryGetValue        25 ns
    Dictionary.TryGetValue    10 ns
    Map.find (allocates)      45 ns

**So the record-representation change is not worth doing for speed.** Moving `DRecord`'s
`Map<string, Dval>` to a `Dictionary` -- a wide change, touching every construction, read and update
-- would save 15 ns per read, which at 7,713 reads per view is **0.12 ms of 33**. The item further
down about records and dicts sharing `Map<string, Dval>` should be read with this attached.

What the same measurement *did* pay for: `Map.find` allocates a `Some` and `TryGetValue` does not,
which is 185 KB per view build and is now fixed.

**A field read does not get slower as the record gets bigger.** A 5-field record's last field reads in
the same time as a 2-field record's, three runs each. Anyone reaching for "flatten records into arrays
indexed per type" should know the tree walk was never the cost.

**`optime` drifted within a single run, by more than several of this round's findings. Now fixed.**
The baseline was **~4,000 ns at the top against ~3,250 at the bottom**, repeatably, and the same
expression measured near the top and at the end read ~5,600 and ~4,500 -- a positional bias of roughly
**0.75 us**.

The cause was the *process* warming, not the row: `measure` already warms each row, and doing more of
that changed nothing. A `saturate` pass before the first row -- 40,000 iterations exercising a fold, a
lambda, package and builtin calls, a record and a list -- removes it. The two baselines now agree to
within ~0.25 us, which is the instrument's noise floor. A first attempt at 4,000 iterations did
nothing, which is worth knowing: the table's own work is ~380,000 iterations, so a warm-up has to be a
real fraction of that.

**What remains is first-touch, and it is small.** The first row to reference a value reads ~0.4 us
above later ones. A third copy of the same row in the *middle* of the table read the same as the one
at the end, which is what separates first-touch from position. The two `load a val (an Int)` rows stay
in, at both ends, as the standing check: ~0.4 us apart is expected, more than that means the drift is
back.

**Absolute `optime` numbers recorded before this are not comparable with ones after.** Adding the
warm-up moved the baseline from ~4,000 to ~3,000 ns by removing bias, not cost.

That is bigger than the elision overhead (0.5 us), the argument type check (0.25) and the frame
dictionary (0.06) -- all of which this round has quoted.

It does **not** touch any of them, because every one was measured by ablation: the same row, in two
builds, in the same position. That is the sound comparison and it is the one to keep using. What is
*not* sound is subtracting two rows far apart in one table, which is exactly how a cost model gets
built by eye. Subtract the nearer baseline, and treat a difference under ~0.75 us as unmeasurable
here.

`load a val (an Int)` is now measured twice, at both ends, so the drift stays visible. If the two ever
agree, it is gone.

**And two rows were measuring more than their names said.** `read a record field` was `preBuilt.a`,
where `preBuilt` is a `val`, so it measured loading a package value and then reading a field -- and it
is how "record field access at 4.6 us" became a ranked target in the campaign brief. `build a 2-field
record` also read a field back. Both now say so. Every list and string row also wraps its result in
`List.length` or `String.length`, which is an elided wrapper around a builtin, carried on top of the
operation the row names.

**Swept the interpreter for the tuple-match trap; one hot site, now fixed.** `assignLetPatterns`
matched `pats, dvs` as a pair, and every tuple destructure reaches it -- including the pairs that are
nearly all of them, where it is called with two empty lists and allocated a pair to answer `true`
about nothing. **2,071,180 -> 2,017,644 bytes per view**, gate 7.3 -> 7.2 MB. Of the three sites using
the tuple form, that was the only hot one: `sameType` runs once per package fn and is cached, and the
third is a comment warning about the other two.

`System.Tuple<Dval, Dval>` no longer appears in a view build's allocation profile at all.

**Negative result: `String.Normalize` is already free on normalized input.** `stringAppend` runs 728
times per view and normalizes every result, which looked like an obvious guard to add. Measured with a
throwaway builtin, 100 calls each against a zero-call row: `s.Normalize()` **5 ns**,
`if s.IsNormalized() then s else s.Normalize()` **5 ns**, no normalize at all 0. .NET short-circuits
already. 728 appends is 3.6 us of a 32 ms view build.

**Where a view build's allocation now sits**, from `alloc-profile`: `System.String` 29% (output text
the CLI builds, not interpreter), `FSharpList<Dval>` 9% (list values themselves), record and dict
construction through `Map<string, Dval>` ~17% between `MapTreeNode`, `FSharpMap`,
`Tuple<string, Dval>` and the field lists, `BigInteger` 3%. Nothing dominant, and the record share is
worth ~1 ms of time (549 constructions at ~1.75 us), which is consistent with field *reads* not being
the cost either.

**An elided forwarder now skips the package call path entirely, not just the frame.** It was still
paying the fetch, `resolveTypeArgs`, `callPackage` and `callPackageResolved` to reach the same
`callBuiltinResolved` -- and for a forwarder nothing but the hash is ever used, which `Apply` has
before it fetches anything. `thinWrapperCachedFor` answers from the cache only; `thinWrapperOf` keeps
the version that can detect, since detection needs the `PackageFn`. First call goes the long way and
fills the cache; every later one skips it.

    a wrapper's overhead over the builtin it wraps   0.5 -> 0.25 us
    viewAtSize                                     32-33 -> 31-32 ms

3,772 elided calls per view, so the ~1 ms observed is the 0.25 us. Prediction and measurement agreeing
is the point of quoting both.

**Round 6, where it stands.**

    viewAtSize        76 -> 31 ms
    keystroke        141 -> 80 ms end to end
    steady.dark      353 -> 35 ms, 7.8 -> 7.3 MB
    bytes per view          2,020,043
    suite: lists 276 -> 39 ms, dicts 130 -> 57, strings 63 -> 25, records 25 -> 15,
           recursion 1181 -> 676
    HTTP     76.31 -> 57.32 KB per request, ~5,200 req/s
    crosslang  Dark 11.84 -> 9.90 KB/iter against node 1.72

**What is left is one sized item and a lot of nothing.** The compile-time `Int` opcode is worth ~1.8 ms
of 31 and needs a new instruction with a non-Int fallback, touching `PT2RT`, the interpreter and
instruction serialisation. Everything else measured this round came back smaller than it looked: the
type checks, the TST, tracing, the frame dictionary, the record representation, `String.Normalize`.
The profile is flat in both time and allocation, and the remaining buckets are all "what a call
costs", spread over thousands of calls with no single hot spot.

**An env-gated probe only fires if its name starts with `DARK_`.** `scripts/run-in-docker` propagates
exactly those. Four ablation switches named `ABL_*` never reached the process, and each reported the
thing it was ablating as costing nothing. **A probe that does not fire is indistinguishable from a
probe that finds nothing** -- the worst failure mode this method has. Check that the switch changes
*behaviour* before trusting that it changed no *timing*: `DARK_ABL_PLUMB=1 run-cli eval
'Builtin.boolNot 5'` should stop reporting a type error, and did not.

**`optime`'s resolution was 250 ns, and most of this round's small findings are one quantum.** It
times in milliseconds over `iterations`, which was 4,000. Every "0.25 us" in the entries above --
elision overhead, the argument type check, the frame dictionary -- is a single step, which is not
distinguishable from noise. `iterations` is 10,000 now, so 100 ns, and the table still runs in 13
seconds.

**With both fixed, the type machinery is the largest item left.** A trivial builtin call -- `boolNot`,
which has no body to speak of -- splits as:

    total                     1.70 us
    argument type check       0.30
    result type check         0.20-0.30
    type-symbol-table work    0.20

~45% of a builtin call, across ~7,200 calls in a view build: **about 5 ms of 31**. That is a real
target, not the "nothing here" the quantised readings suggested, and it is where a round 7 should
start rather than at the compile-time opcode (1.4 ms at the corrected resolution).

The safe corner of it is done: the builtin path merged `explicitlyBound` into the TST unconditionally
where the package path already guards on it being empty, which it is on every call without explicit
type args. 1.70 -> ~1.57 us per trivial builtin call, not visible in `viewAtSize` because 7,200 calls
x 0.13 us is 0.9 ms and that instrument's spread is wider.

The rest needs care: a builtin whose signature has no type variables at all could skip the TST work
entirely and check arguments by `Dval` case rather than by unification. That is a second type checker
unless it is written as a fast path *inside* the existing one, and getting it wrong accepts bad values
rather than merely running slowly.

**Superseded: the open question is the type checks.** Part of the 2.25 us is checking the arguments against the *wrapper's* declared types and the
result against its declared return type. Skipping those is where the time is, but it changes what
`Stdlib.String.length 5` reports: the wrapper's type error today, the builtin's `incorrectArgs`
after. Worth measuring how much of the 2.25 us is the checks before deciding whether to pay that.

**Target 3 is withdrawn, and it corrects a round 5 claim.** A record field read is **0.75 us above
baseline**, not the 4.6 this round has been quoting. The old `optime.dark` row was `(recFn i).a`,
which is a package call plus a record construction plus a field read -- and it reported *less* than
the bare package call on the line above, which should have been caught at the time. The row now uses
a record built outside the loop. A 5-field record reads the same as a 2-field one, so the `Map.find`
depth is not the cost at these sizes either.

**The round 5 note on `Layout.at` is wrong in its explanation, not its result.** It attributed most
of that helper's 52 us to "five record-field reads at ~4.6 us each". At 0.75 us those five are under
4 us of the 52. The fix -- building border spans directly -- measured a real 9 ms and still stands;
only the reason given for it was wrong.

**What the corrected table actually ranks.** Above a 20.75 us baseline: calling a package fn **+4.5
us**, applying a lambda **+4.0**, updating a record field **+5.25**, building a record **+1.75**,
reading a field **+0.75**. So the per-operation cost worth attacking is *calls*, not field access --
which is the same conclusion target 2 reached from the other direction, and where `List.fold`'s
package call per element already put it.

**What target 1 actually buys, and what it costs -- measured, and it decides the order.** Ran the
suite and the CLI with native `map` in, against the same build without it.

| workload | alloc before | after | time before | after |
|---|---|---|---|---|
| `lists` | 11.79 KB | **27.75** | 347 ms | **226** |
| `strings` | 6.34 KB | **8.89** | 84 ms | **64** |
| `dicts` | 12.57 KB | 11.93 | 175 ms | 169 |
| `containers` | 3.39 KB | 3.67 | 13 ms | 14 |
| `json`, `records`, `recursion` | flat | flat | flat | flat |

And the CLI: `viewAtSize` **75 -> 69 ms**, repeatable, about 5% of a keypress.

So native `map` is a genuine trade today: **`lists` 1.5x faster and 2.4x more allocating.** That is
not "a change that only moves wall-clock", which the top of this document allows; it is spending
rounds 1-4's work to buy round 6's.

**Which settles it: target 1 must not land until target 2 is closed, and closing it is not more
trimming.** Eight cuts took the per-application cost from 73.6 MB to 11.4 on the amplifier, and the
~350 bytes left is the interpreter's own `task { }` and `Task` per nested execution -- the thing a
nested run inherently pays for being a *separate* execution. Removing it means what target 2 always
said: push a frame on the VM the builtin is already running inside and run until it pops, rather than
starting an execution. Every cut so far has been making the separate execution cheaper; the remainder
is the separateness itself.

**Still 1.4x over, ~350 bytes an application.** A GUID, the `callFrames` clear-and-insert, the
`NEList.singleton` the caller builds per element, and the `Result` and `Ply` wrappers. Target 1 needs
this under the budget, so it stays blocked -- but the gap is now four times smaller than the budget
itself, where it started at nine times larger.

**Superseded: ~730 bytes an application.** What is left is the `task { }` in `executeApplicable`
itself with its try/try/with/finally, the `Task` and `Result` it returns, a GUID, and the
`NEList.singleton` the caller builds per element. Removing the builder needs a synchronous path for
the case where the lambda does not await -- returning `Ply<ExecutionResult>` rather than
`Task<ExecutionResult>`, which ripples to `Stream.fs`, `HttpServer.fs` and `DB.fs`.

**Safety note, checked rather than assumed:** `CallFrame.argBuf` documents that its reuse is safe
because "the builtins that invoke a lambda directly build a whole new VM". Pooling preserves that --
a VM is out of the bag for as long as it is running, so a nested application takes a different one --
but it is now an invariant of the pool, and if the pool ever hands the same VM out twice, `argBuf`
corrupts silently.

**Target 2, first half done: pool the VM instead of building one per lambda application.**
`VMState.reuseFor` re-points a finished VM at a new tiny program, and `ExecutionState` carries a
`ConcurrentBag` of them; `executeApplicable` takes one when it can. Only a VM that ran to completion
goes back in the bag, since one that raised may still hold frames. The per-VM lambda caches are kept
deliberately -- keyed by expression id, so reuse warms them across applications rather than throwing
them away every call, which was a separate complaint below.

Measured through the native-`map` amplifier (one VM per element, so `steady.dark` builds 10,000):

| | gate |
|---|---|
| native map, before any of this | 843.7% over |
| + shared disabled stats | 843.1% (gate runs telemetry-on, so no effect there) |
| + VM pooling | **257.0% over** |

73.6 MB down to ~27.9. Not enough to land target 1 yet: ~2 KB an application remains, which is the
instruction list rebuilt per call (`LoadVal` per argument, assembled with list appends), the
`List.toArray` of it, the registers array, the `CallFrame` and a GUID.

**The next cut is obvious from that list.** The `LoadVal` instructions exist only to get the callable
and the arguments into registers. Writing them into the registers directly leaves a single `Apply`
instruction, whose array can be cached per arity and shared across every call. That removes the list
building, the array conversion and most of what is left.

**Honest about what is landed:** nothing hot uses `executeApplicable` today -- the CLI's lambdas go
through `List.map`, which is Dark -- so this has no user-visible effect on its own and no instrument
outside the amplifier can see it. It is landed because it is measured through that amplifier and it
is the thing standing between round 6 and its first real win. `executeFunction` was left alone: it
routes through `executeExpr`, which also serves top-level runs whose stats the gate reads.

**Sized target 2: stats are half of it, and trimming the other half will not be enough.** Used native
`map` as an amplifier -- it builds one VM per element, so `steady.dark` builds 10,000 and the gate
reads the per-VM cost magnified. Baseline with native map: 73.6 MB, 843.7% over budget. Probed by
making `InterpreterStats.create()` always return one shared instance (wrong, but it bounds the
share): **430.7%**, so ~41.4 MB. Stats are about half the per-VM cost; the rest is the two GUIDs, the
instruction and register arrays, five `Dictionary`s and a `ResizeArray`.

**Which settles the approach.** Even with stats free, native map is still 5.3x over budget. There is
no version of trimming `VMState.create` that makes building one per element acceptable. Target 2 has
to be what it always looked like: push a frame on the VM the builtin is already running inside, the
way the `Apply` opcode does, and construct nothing.

**Landed, unverifiable, honest about it:** `InterpreterStats.create()` now returns a single shared
instance while counting is off. Every write to those counters is behind `if vm.stats.enabled`, so
sharing is safe, and a fresh set is thirteen `Dictionary`s and seven arrays per VM. But
`scripts/perf/gate` runs with `DARK_TELEMETRY=1`, so it never takes that path and cannot see the
change; it is landed on the reading of the code, not on a measurement, and it becomes measurable the
moment target 2 makes VM creation common. Worth noting the corollary: **every gate number includes
per-VM stats allocation that a real run with telemetry off does not pay.**

**Target 2 is a prerequisite for target 1, not an enhancement. Measured the hard way.** Landed a real
native `List.map` -- correct types, correct error propagation, backend suite green at 10,341, and
`map over 5` went 120,250 -> 76,000 ns as predicted. Then `scripts/perf/gate` failed at **73.6 MB
against a 7.8 MB budget, 9.4x over**. Reverted.

The cause is exactly the `executeApplicable` finding below: it boots a fresh `VMState` per call, and
`steady.dark` maps over 50 elements 200 times, so that is 10,000 VMs, each with two GUIDs, ~13
dictionaries and 7 arrays. Native map trades five interpreter operations for one builtin dispatch
plus one VM construction, which is faster in time and far worse in bytes.

**So the order is fixed: make applying a lambda from a builtin cheap first, then the list primitives
become worth landing.** Doing them the other way round ships a 9x allocation regression that a
time-only instrument cannot see -- `optime.dark` reported the win and said nothing about the cost.
This is the clearest case this campaign has produced for the playbook's rule about measuring the
thing you are not looking at.

**Probed: a native `List.map` is worth ~2x on the operation, not 10x.** Wrote a throwaway
`listMapProbe` builtin that applies the lambda with `Exe.executeApplicable` per element, measured it
against the Dark `map`, then reverted it. Over 5 elements, above the 21.5 us baseline: Dark map
**+99 us**, native map **+52 us**. About 9.4 us an element saved.

The residual says where the ceiling is. Native map's remaining ~10 us an element is the
`executeApplicable` call, and an *inline* lambda apply measures 4.1 us. So going native trades five
interpreter operations for one builtin dispatch plus one applicable call, and that applicable call
is more expensive than an ordinary lambda application. **If `executeApplicable` were as cheap as an
inline apply, native map would be 3-4x rather than 2x** -- worth measuring before assuming the
builtin is the whole answer.

Precedent exists: `Param.makeWithArgs` takes a `TFn`, and `Stream.fs` and `HttpServer.fs` already
apply Dark lambdas from builtins this way. `Param.make` asserts *against* function types, which is
why nothing in `List.fs` takes one today.

**Why `executeApplicable` costs 10 us: it boots a VM per call.** Read rather than guessed. Both it
and `executeFunction` build a small instruction sequence (`LoadVal` for the callable, one per arg, an
`Apply`) and then call `RT.VMState.create`, which allocates, *per lambda application*:

- two fresh `Guid.NewGuid()` values (thread id and root call frame id),
- the instruction array and a registers array,
- five `Dictionary`s -- `callFrames`, `lambdaInstrDataCache`, `lambdaEpCache`, `pendingCallArgs`,
  `framePool` -- plus a `ResizeArray`,
- and `InterpreterStats.create()`, which is another eight `Dictionary`s and six `Array.zeroCreate 32`.

So applying a one-argument lambda from a builtin allocates roughly thirteen dictionaries, seven
arrays and two GUIDs. The interpreter's own `Apply` opcode instead pushes a frame onto the VM it is
already running and finds the lambda in `exeState.lambdaInstrCache`. That is the whole 4.1 us versus
10 us.

Three consequences beyond the timing:

- **Every lambda-taking builtin pays it per application**, not per call: `Stream.unfold` pays it on
  every pull, and `HttpServer` and `DB` have the same shape.
- **`InterpreterStats` cannot see any of it.** The fresh VM gets fresh stats and nothing merges them
  back, so work run through `executeApplicable`/`executeFunction` is invisible to the counters. The
  numbers earlier in this section are unaffected -- the CLI's lambdas go through `List.map`, which is
  Dark, so its applies run on the main VM and are counted -- but anyone measuring a lambda-taking
  builtin should know.
- **The per-VM lambda caches are thrown away each call**, so `lambdaInstrDataCache` and
  `lambdaEpCache` never warm across applications.

The fix is to apply on the caller's VM by pushing a frame, rather than constructing one. That is an
interpreter API change and wants its own pass. A cheaper partial: `InterpreterStats.create()` is
unconditional even though `enabled` is read at construction, so the eight dictionaries and six arrays
are allocated whether or not anything will record into them.

**Not landed, and the reason is types.** The Dark `map` gets its result element type from the values
it built; the probe returned `VT.unknown`, which is fine for a timing probe and wrong for the
language. A real implementation has to compute the result `ValueType` the way `listFlatten` does,
and `map` is used everywhere, so getting that wrong is a type-checking bug rather than a slow path.
That is the next piece of work, not a five-minute change.

**That is the target for an interpreter round, and it is narrow.** `push` and `reverse` are already
builtins; `fold`, `map`, `filter` and `range` are not. The CLI's renderers are `map`/`filter`/
`flatten` pipelines almost end to end, which is why every structural win this round has been "run
fewer list operations". Making the iteration primitives native attacks all of them at once.

**This qualifies a closed item.** "Making stdlib wrappers free" was closed with "a warm package call
allocates *nothing*", which is true and was measured. In *time* a package call costs ~5.1 us, and
`fold` makes one per element. The old conclusion was right about bytes and says nothing about
latency; both belong on the record.

**Done: `composeRow` notices when a row is already in column order.** `renderSegments` walks
segments left to right, so it sorted them first -- with `sortBy`, which is two Dark `map`s around a
native sort, about 240 us a row. But a renderer produces a row left to right, so it was almost always
reordering data that was already ordered. `overlay` appends, and trimming an earlier segment leaves
it where it was, so the result is in column order exactly when the spans arrived that way; tracking
that is one comparison against the last column seen.

| | before | after |
|---|---|---|
| `Canvas.toView` | 53 ms | **48** |
| `viewAtSize` | 82 ms | **74** |
| keypress, end to end | 141 ms | 141 |

Probing said removing the sort entirely was worth 10 ms; half of that goes back into the tracking,
because carrying a three-part accumulator costs a tuple build and destructure per span. Using the
last column rather than a running maximum saved another millisecond by dropping an `Int.max`, which
is a package call at ~5 us and was running once per span.

End to end did not move. The phase numbers repeat exactly (48/48/48 across runs), so the 8 ms is
real; `keystroke`'s spread at this point is wider than the win.

Checked rather than assumed: spans fed in ascending, descending and shuffled order all compose to the
same row, so the sort still runs when it is needed.

**Done: border spans are built directly, not through `Layout.at`.** `Layout.at` measures the text,
clips it to the region and bounds-checks the position: right for arbitrary content, all wasted on a
one-character bar at a position `Box.draw` just computed. Measured at **52 us a call above harness
floor**, and only 7 us of that is `styledWidth` -- the rest is five record-field reads at ~4.6 us
each, the `Int.max`, the `Span` record and the single-element list. A panel draws two per row.

| | before | after |
|---|---|---|
| `renderFull` | 46 ms | **38** |
| `viewAtSize` | 97 ms | **89** |
| keypress, end to end | 159 ms | **150** |

Same 252 spans out; they are just cheaper to make. The bounds check `Layout.at` would have done is
now one check per panel rather than one per row, since every row of a panel is in bounds or none is.

**Done: `isFirstRun` asked for items when it wanted a yes/no.** Home's detail pane and its title both
call it, per frame. It answers "does this account have any packages of its own" with
`List.isEmpty (loadItems ...)`, and `loadItems` materializes every matched item's full definition
*and pretty-prints its type signature* to build the list it then measures the length of. There is
already a cheap variant, `allDirectDescendantNames`, documented as what completion wants for exactly
this reason.

It depends entirely on whether the account's module has anything in it:

| owner module | items (before) | names (after) |
|---|---|---|
| populated | 32.6 ms | **7.5 ms** |
| empty | 4.4 ms | 4.4 ms |

**And that is a caveat on this whole section.** The workloads build state with a made-up owner name,
whose module is empty, so they measure only the cheap column -- which is why this fix reads as flat on
`keypress.dark`. `scripts/perf/keystroke`, which drives the real CLI, agrees with the workloads at
~150 ms, so the empty case is the representative one and the round's numbers stand. But an account
that has authored packages pays the other column on every frame, and nothing in the harness would
have shown it. The workloads now say so in a comment.

**Done: `isFirstRun` asks an existence question, so it does an existence query.** It wants to know
whether the account has any packages of its own. It was asking a *search*, which scans `locations`
four times for a fixed ~4.4 ms whatever it finds. `pmOwnerHasItems` is an equality seek on the owner
index -- `EXPLAIN QUERY PLAN` says `SEARCH locations USING INDEX idx_locations_owner_modules
(owner=?)`.

| | before | after |
|---|---|---|
| `isFirstRun` | 4.50 ms | **0.16** |
| `previewLines` | 8.66 ms | **4.03** |
| `renderFull` | 38 ms | **28** |
| `viewAtSize` | 89 ms | **82** |
| keypress, end to end | 150 ms | **141** |

Home calls it twice a frame (detail pane and pane title), so the frame saves both.

**Negative result: indexing the search's predicate does not help.** Tried on a copy of the store.
Dropping the dead `OR` branch leaves `owner || '.' || modules LIKE 'X.%'`, still a computed
expression, still `SCAN`. Adding an expression index on `owner || '.' || modules COLLATE NOCASE` did
not change the plan either -- SQLite kept the covering scan, with `LIKE` and with `GLOB`. So the seek
is not available by indexing, and the search's floor is simply four scans.

Which also right-sizes the scan: **5,914 rows scan in ~1 ms**, four of them make the 4.4 ms. There is
nothing pathological in any single query. The remaining candidate is collapsing the four into one
with a kind discriminator, worth ~3 ms a search; the `isFirstRun` fix above avoided the search
entirely, which was better.

**A package search costs a fixed ~4.4 ms, whatever it finds.** Measured through
`allDirectDescendantNames` on this store: a module that does not exist (0 results) 4.4 ms, the root
(3) 5.3, `Darklang` (23) 6.4, `Darklang.Stdlib.List` (53) 4.8. Result count barely moves it, so it is
a floor, not a per-row cost. It matters because `isFirstRun` runs a search per frame -- twice, since
Home's detail pane and its title both call it -- and completion, listings and nav all search too.

Where it goes:

- **One search is four SQL queries** (submodules, types, values, fns), each hitting `locations`,
  which holds 5,914 rows here. Timed directly in sqlite3, one of them is ~2 ms.
- **Each is a scan, not a seek.** `EXPLAIN QUERY PLAN` says `SCAN l USING INDEX idx_locations_module`
  -- the index is only avoiding the table read. The predicate is
  `(modules LIKE 'X.%') OR (owner || '.' || modules LIKE 'X.%')`, and an `OR` over a computed
  expression cannot seek.
- **The first branch of that `OR` matches nothing.** All 5,914 rows have `modules` not starting with
  `owner` (`Darklang` + `Cli`, never `Darklang` + `Darklang.Cli`). So in this store the scan exists to
  evaluate a condition that is always false.

Three candidate fixes, largest first, none taken yet:

1. Confirm the dead `OR` branch is genuinely dead, and if so drop it. That leaves a single prefix
   predicate on the computed `owner || '.' || modules`, which an expression index could serve as a
   range seek instead of a 5,914-row scan.
2. Collapse the four queries into one with a kind discriminator: ~3 scans saved per search.
3. Call `isFirstRun` once a frame rather than twice, worth 4.4 ms and needing no DB change -- but the
   two callers sit either side of the generic preview path, so it wants a little plumbing.

**Negative result: `allDirectDescendantNames` is not cheaper on an empty module.** Both it and
`loadItems` cost 4.4 ms there, so the saving is entirely the materialization, not the search. The
search itself is 4.4 ms for a module that does not exist, which is its own question.

**Negative result: the nullary-constant sweep is exhausted here.** After three wins from that
pattern, grepping the render path for the rest of it finds nothing worth taking:
`sidebarIsColumnNow` is 30 us a call (it makes a syscall for the terminal size) and `extensionViews`
45 us, each called about four times a frame -- ~0.2 ms together against an 89 ms view build. Both
correctly remain functions.

**Pane borders are 86% of the frame's spans and 85% of the time to composite it.** The single
biggest structural fact found this round. `Box.draw` emits the vertical sides as two spans per row --
a colorized `│` at each edge -- so a full-height panel is 76 spans and the workbench draws three
panels. Probed by dropping `sides`: spans 252 -> 36, `Canvas.toView` 53 -> **8 ms**.

That also explains the size curve from the top of this section. 80x24 is ~95 ms and 120x40 ~284
because the narrow terminal drops panes, not because it has fewer cells; and 280x60 is barely worse
than 200x50 because the pane count is the same and only the row count grew.

Nothing cheap fixes it. Compose costs ~200 us a span and a border span is already one character;
there is no fat in it. The options are architectural: draw the vertical chrome as part of each row
rather than as spans, or cache it, since it depends only on region size and focus and is identical
frame to frame. Both are bigger than an afternoon. The alternative is the per-operation interpreter
cost, filed under "Larger, not yet scoped".

Two smaller redundancies noticed while measuring, neither worth a commit alone: `Layout.at` measures
a span with `styledWidth` and then `composeRow` measures the same text again, so every span is
measured twice; and `Layout.at` returns a single-element list per span that is then flattened.

**The body pane is two thirds of the view build.** Probed by stubbing `renderBody` to `[]`:
`renderFull` 49 -> 20 ms and `Canvas.toView` 54 -> 19, with spans falling 252 -> 86. So the body
costs ~29 ms to build and ~35 ms to composite, ~64 ms of the ~100 ms view. The sidebar, context row,
hints and overlays are the other third. Next: decompose `renderViewBody` (Home is a split pane, list
plus detail) the way the sidebar was.

**Negative results in `Canvas.toView`, so nobody re-probes them.** `styledWidth` costs ~4 us a call,
1 ms across all 252 spans -- it is native and it is not the cost. The per-row floor is 1 ms for 40
empty rows. Bucketing all 252 spans is 3 ms. Spans are evenly spread at 6-9 a row with no renderer
emitting many tiny ones, so there is no span-count pathology to fix. What is left is ~20 ms in
`renderSegments` and ~9 ms in the residual `overlay` walk, both volume rather than structure.

**Done: the sidebar key handler computed the outcome of keys nobody pressed.** `let` is eager, so
`handleSidebarKey` opened by binding `rows = sidebarRows state 0 0` (the most expensive thing the
handler touches), then `openSel`, `collapseSel` and `back` -- and `openSel` calls `goTo`, which loads
items from the DB. All of it ran on every keypress and was discarded unless that exact key was
pressed. Stepping into the content pane used none of it.

They are nested functions now, so each key does only its own work.

| | before | after |
|---|---|---|
| `handleKey` (the whole state transition) | 11 ms | **0** |
| keypress, end to end | 163 ms | 159 |

The state transition phase is gone. End to end moved ~4 ms, inside the noise band, because the
handler is a small share of a keypress once the view build dominates -- but the phase instrument is
unambiguous and repeats exactly.

This is the third time this round the same bug has appeared: a value bound eagerly that only some
paths need. `visibleViews` per workspace, `workspaces` and `viewSpecs` as nullary functions, and now
a whole key handler's worth. Worth grepping for others.

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

- ~~`alloc-profile` cannot run: `dotnet-trace` is not installed in the container.~~ Fixed: the
  Dockerfile installs it next to `fantomas`. It runs, and says what the header predicts -- the type
  profile of `steady.dark` is entirely flat, every entry one tick.
- **There is still no CPU profiler, and `dotnet-sampled-thread-time` is not it.** Tried:
  `dotnet-trace collect --profile dotnet-sampled-thread-time --format speedscope` over the lambda
  amplifier attributes 100% of the time to `UNMANAGED_CODE_TIME` with every managed frame at 0.00%,
  so there is no signal to rank. `cpu-sampling` and `thread-time` are `collect-linux` profiles and
  need kernel perf events, which the container does not have. This matters more each round: the
  campaign is now chasing cycles rather than bytes (see the let-pattern note), and every remaining
  finding is inferred from A/B timings rather than read off a profile.
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
