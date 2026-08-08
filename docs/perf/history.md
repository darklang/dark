# Dark performance: what happened, and the numbers

The durable record of the performance campaigns. Round-by-round numbers, what each round found, and
the things learned that are worth not re-learning. Working notes for each round have been deleted;
this is what survived them.

Method: `docs/perf/playbook.md`. What's next: `docs/perf/roadmap.md`.

---

## The arc

The reference workload is `scripts/perf/workloads/steady.dark`: 200 iterations of
`List.range 1 50 |> List.map (+1) |> List.length`. Whole-process allocation, Debug, unless noted.

| point | allocation | body time |
|---|---|---|
| before round 1 | not instrumented | 2,571 ms |
| after round 1 (#5696) | 273.1 MB | 391 ms |
| after round 2 | **8.0 MB** | 342 ms |

Release: **211.99 MB -> 7.6 MB**, and 256 ms -> 177 ms.

Round 3 (NativeAOT) is a separate axis: it doesn't change allocation, it deletes JIT compilation.
One-shot commands land 4.6-8x faster on top of the above, and the six steady-state workloads come
out 1.3-2.3x faster as well. Details below.

Per iteration, Release, all three points rebuilt and measured on today's harness. The pre-round-1
figures needed the commit rebuilt with the allocation reading added and *nothing else*, since the
telemetry that reports it was added by round 1:

| workload | before round 1 | after round 1 | after round 2 | total |
|---|---|---|---|---|
| `recursion` | 13,436 KB | 2,470.6 KB (5.4x) | **0.48 KB** (5,147x) | **27,993x** |
| `lists` | 3,345 KB | 865.3 KB (3.9x) | **12.0 KB** (72x) | **279x** |
| `strings` | 684 KB | 191.5 KB (3.6x) | **6.6 KB** (29x) | **104x** |
| `dicts` | 1,055 KB | 369.2 KB (2.9x) | **15.7 KB** (23.5x) | **67x** |
| `records` | 202 KB | 62.6 KB (3.2x) | **8.3 KB** (7.5x) | **24x** |
| `json` | 76 KB | 33.4 KB (2.3x) | **13.0 KB** (2.6x) | **5.8x** |

Time, Release, 200 iterations each. The AOT column is round 3, measured on a published NativeAOT
binary from the same commit as the R2R one; re-measuring R2R at that commit reproduced the round-2
column within a few percent, which is what makes the two comparable:

| workload | before round 1 | after round 1 | after round 2 | after AOT | total |
|---|---|---|---|---|---|
| `recursion` | 53,389 ms | 616 ms (87x) | 501 ms (1.2x) | **385 ms** (1.3x) | **139x** |
| `records` | 823 ms | 25 ms (33x) | 16 ms (1.6x) | **9 ms** (1.8x) | **91x** |
| `strings` | 905 ms | 73 ms (12.4x) | 48 ms (1.5x) | **27 ms** (1.8x) | **34x** |
| `json` | 242 ms | 17 ms (14.2x) | 13 ms (1.3x) | **6 ms** (2.3x) | **40x** |
| `lists` | 2,127 ms | 244 ms (8.7x) | 190 ms (1.3x) | **110 ms** (1.8x) | **19x** |
| `dicts` | 1,073 ms | 143 ms (7.5x) | 103 ms (1.4x) | **72 ms** (1.4x) | **15x** |

Allocation per iteration is unchanged by round 3, within 4% and in both directions: AOT changes
when code is compiled, not what the interpreter allocates. The allocation table above still stands.

Round 1 took most of the time; round 2 took most of the allocation.

Careful with older documents: round 1's own notes quote the reference workload at 107.1 MB at its
end, and ~868 MB before it. Those were a *different, lighter* workload, replaced during round 2 when
the old one stopped being representative. Rebuilt and re-measured at the round-1 merge point,
today's workload reads 273.1 MB. Only compare numbers produced by the same harness -- which is why
the table above was re-measured rather than assembled from notes.

Per iteration, against the same program in other runtimes: python 0.47 KB, node 1.70 KB, Dark
**12.0 KB** -- 7.1x node, from ~700x at the start of round 2.

## Round 0: make the numbers trustworthy

Before any optimisation was allowed, four things had to be true: the measurements had to be
repeatable, the profiler had to be honest, the store had to be a fixed fixture, and a change had to
be attributable. Getting there found that the profiler was lying in two separate ways and that
package changes cannot be A/B tested at all, because `PackageRefs` makes a binary and a store a
matched pair. That constraint explained a day of confusing results and still holds.

The lasting output is the instrumentation: per-stage allocation counters, per-opcode counters, and
the telemetry that everything since has been steered by.

## Round 1 (#5696): the first pass through the interpreter

868 -> 273 MB. Dev-time CLI runs ~3.3x faster; per-keystroke typing latency down ~2x.

What it established, and what's still true:

- **The interpreter is allocation-bound**, not compute-bound. That framing drove everything after.
- **A Dark function call allocated ~8 KB.** Chasing that one number produced most of the round.
- **Runtime type checking is two jobs wearing one coat**: checking, which could in principle move to
  compile time, and instantiation, which is load-bearing and has to stay. Ablation put checking at
  -20% allocation and -14% wall.
- **NativeAOT is the single biggest available result** and needs no code change, only CI.
- Five `List` functions were made native, which is why `List.push` is a builtin today.
- `dark version` makes a network call costing ~700 ms.

Two corrections it made to itself, both worth remembering: an early note claimed a per-region
allocation figure that later turned out to be measured with brackets spanning awaits (so they
counted nested execution, not the region), and the "8 KB per call" figure superseded an earlier,
wrongly-scaled one.

## Round 2: the rewrite, and the course correction

273 -> 8.0 MB Debug, 212 -> 7.6 MB Release. The round-2 PR description has the full write-up,
including every individual win.

The shape of it: half a round of more-of-round-1 (representation changes, the biggest being the
interpreter loop moving off Ply onto F#'s resumable-code `task`, worth -51.2 MB on its own), then a
stop to build proper instruments after returns started diminishing, then a second half driven
entirely by what those instruments found.

The correction was the important part. Every number in both campaigns had come from one script. When
six workloads finally existed, **that script turned out to be the second-cheapest of them** -- a
plain function call cost five times a list iteration, an HTTP request nine times one, and the HTTP
profile was dominated by type-checker paths the list workload never touched.

## Round 3: NativeAOT

Measured 2026-08-08, against the round-2 merge point. Both binaries built from the same commit, so
this isolates the publish mode and nothing else. Startup timings are paired A/B through
`scripts/perf/bench`, interleaved, 9 pairs, and AOT won 9 of 9 on every scenario.

Round 2 shrank the work; this round deletes the JIT that was compiling it. They compose.

### One-shot command latency, Release

| scenario | R2R (ships today) | AOT | reduction |
|---|---|---|---|
| `status` | 187 ms | **24 ms** | -86% |
| `eval-trivial` | 200 ms | **26 ms** | -86% |
| `help` | 199 ms | **32 ms** | -84% |
| `eval-map1000` | 253 ms | **40 ms** | -83% |
| `eval-listheavy` | 286 ms | **62 ms** | -78% |

The gradient is the point: AOT deletes a fixed cost, so the win shrinks as real interpreter work
grows. 8x on a command that mostly starts up, 4.6x on one that mostly computes.

### Steady-state throughput, Release, per the round-2 workloads

The roadmap flagged this as the thing to measure rather than assume, since AOT compiles ahead of
time and can't re-optimise hot loops. On these six it does not regress. It is faster on all of them:

| workload | R2R ms | AOT ms | | R2R alloc/iter | AOT alloc/iter |
|---|---|---|---|---|---|
| `json` | 14 | **6** | 2.3x | 12.44 KB | 12.73 KB |
| `lists` | 198 | **110** | 1.8x | 11.76 KB | 11.89 KB |
| `records` | 16 | **9** | 1.8x | 8.09 KB | 8.42 KB |
| `strings` | 48 | **27** | 1.8x | 6.74 KB | 6.46 KB |
| `dicts` | 101 | **72** | 1.4x | 15.91 KB | 15.60 KB |
| `recursion` | 490 | **385** | 1.27x | 0.32 KB | 0.32 KB |

But 200-iteration bodies are short enough that a JIT never reaches its best steady state, so this
alone doesn't answer the roadmap's question. The server does.

### The long-running case, where the roadmap's fear is real (a bit)

`scripts/perf/http`, 20,000 requests at concurrency 32, about 11 seconds of sustained load. Three
paired runs, alternating:

| run | R2R | AOT |
|---|---|---|
| 1 | 1,783 req/s | 1,684 req/s |
| 2 | 1,724 req/s | 1,719 req/s |
| 3 | 1,833 req/s | 1,776 req/s |

R2R is ahead in 3 of 3, by 0.3% to 5.6%. The spread within each mode is about 6%, so the gap sits
inside the noise band, but the sign is consistent. Latency moves the same way: p50 16.8 ms against
17.8 ms, p95 27.9 against 29.6. Allocation per request is identical, 99.79 KB against 99.84 KB.

A shorter run (2,000 requests at concurrency 16) shows a dead heat, 1,637 against 1,651. The cost
only appears once the load lasts long enough for the JIT to finish tiering, which is exactly the
mechanism the roadmap named.

So the trade is real and it is small: **give up a few percent of sustained server throughput, get
4.6-8x on one-shot commands.** For a CLI that is obviously the right side of the trade. If `serve`
ever becomes the main way Dark runs, it is worth re-measuring rather than inheriting this decision.

**One confound, stated because it undercuts the number above.** `Cli.fsproj` sets
`IlcOptimizationPreference=Size`, so the throughput deficit was measured against an AOT binary that
was explicitly asked to prefer small code over fast code, and `IlcFoldIdenticalMethodBodies=true`
is on as well. Some or all of the 0.3-5.6% could be those settings rather than anything inherent to
compiling ahead of time. Nobody has measured `Speed`. Do that before treating the gap as a property
of AOT; see the roadmap.

Allocation per iteration is unchanged, within 4% and in both directions. AOT changes when code is
compiled, not what the interpreter allocates.

### Whole-process allocation is 6% higher, and that is startup

| build | `steady.dark`, whole process |
|---|---|
| R2R | 7.7 MB (7.7 / 7.7 / 7.7) |
| AOT | 8.2 MB (8.2 / 8.2 / 8.3) |

Same instruction count both ways (298,236), so it is not extra work. `suite` differences startup out
and shows body allocation identical; `gate` deliberately does not difference, so the gap lands
there. It is startup allocation, and the reason `gate --published` fails against an AOT binary.

**The budget was deliberately not re-pinned.** The roadmap says to re-pin when AOT lands, but CI's
gate runs against the plain Release publish from `build-backend`, which this work does not change.
Re-pin when the published build actually becomes AOT, using 8.2 MB as the starting point, not before.

### The other thing that turned up

`dark version` costs 0.34-0.57 s wall clock, and 0.03-0.05 s with the network unshared. It is
dominated by an HTTPS round trip to check for a newer release. Any timing of `version` measures
GitHub, not us, which is worth knowing before someone quotes it as a startup number.

## Things established that shouldn't be re-derived

Cheap to state, expensive to learn.

**On F# and .NET**

- A `uply` loop allocates per iteration in proportion to the **size of its body**, bind or no bind.
  F#'s built-in `task` allocates nothing. Ply predates F# 6's resumable code; this codebase is on
  FSharp.Core 10.
- A completed `Ply` allocates nothing -- it's a struct. The cost is the *builder*, not the value.
- Ply's builder is **not** resumable code, so unlike `task` it allocates in Release exactly as much
  as in Debug. This is why several CE-heavy paths survived into the shipped binary.
- F# only reduces resumable-code state machines in **optimised builds**. Record-heavy work looks
  ~2.8x worse in Debug than it is. Develop in Debug (two minutes vs ten); decide in Release.
- `FS3511: this state machine is not statically compilable` is an *error* in Release here
  (`--warnaserror`) and silent in Debug. It means the fallback allocating implementation is in use.
  The trigger is a bind inside a nested match arm.
- `match d.TryGetValue k with | true, v ->` allocates a `Tuple<bool,'v>` on every lookup.
- `match a, b with` allocates the pair.
- `let f x = g captured x` is a closure wherever the compiler can't lift it -- and a use inside the
  same function's loop is enough to stop the lift.
- A `let mutable` captured by a continuation becomes a heap ref cell, allocated whether or not the
  branch that needs it runs.
- .NET 10's object stack allocation is **not** quietly removing this work:
  `DOTNET_JitObjectStackAllocation=0` moves the total by 4 KB.

**On Dark's runtime**

- Type names are content hashes, so `type A = { x: Int }` and `type B = { x: Int }` are the **same
  type**. Any test that passes a `B` where an `A` is expected is testing nothing.
- Following from that: if a declared type's name equals the value's type name, the declared type
  cannot be an alias, because a value built through an alias carries the *underlying* name. That one
  observation is what let most of the type checker move to a synchronous path.
- `DInt` is 56 bytes regardless of magnitude, because `DarkInt` is a struct DU carrying space for a
  `bigint`.
- CRLF is a single extended grapheme cluster, so `"a\r\nb"` split on `"\n"` is one part cluster-wise
  and two char-wise. Any ASCII fast path over strings must exclude CR.
- A published binary resolves its data and log directories relative to **its own location**, not
  `DARK_CONFIG_RUNDIR`.
- `build-release-cli-exes.sh` *moves* the published binary into `clis/`, leaving empty the publish
  directory it names in its own log.
- The first run against an empty store seeds it -- hundreds of megabytes. Any measurement harness
  needs a throwaway run first, or that lands in whichever measurement happens to be first.
