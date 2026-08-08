# Dark performance: what happened, and the numbers

The durable record of the performance campaigns. Round-by-round numbers, what each round found, and
the things learned that are worth not re-learning. Working notes for each round have been deleted;
this is what survived them.

Method: `docs/perf-playbook.md`. What's next: `docs/perf-roadmap.md`.

---

## The arc

The reference workload is `scripts/testing/perf-workloads/steady.dark`: 200 iterations of
`List.range 1 50 |> List.map (+1) |> List.length`. Whole-process allocation, Debug, unless noted.

| point | allocation | body time |
|---|---|---|
| before round 1 | ~868 MB | 2,571 ms |
| after round 1 (#5696) | 273.1 MB | 391 ms |
| after round 2 | **8.0 MB** | 342 ms |

Release, measured like-for-like on today's harness: **211.99 MB -> 7.84 MB**, and 256 ms -> 177 ms.

Careful with older documents: round 1's own notes quote the reference workload at 107.1 MB at its
end. That was a *different, lighter* workload, replaced during round 2 when the old one stopped
being representative. Rebuilt and re-measured at the round-1 merge point, today's workload reads
273.1 MB. Only compare numbers produced by the same harness.

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
