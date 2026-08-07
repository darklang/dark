# The same workload in Dark, node and python

The Dark side is `scripts/testing/perf-workloads/suite/lists.dark`: 200 iterations of
`List.range 1 50 |> List.map (+1) |> List.length`. `steady.py`, `steady.js` and `steady.sh` are
faithful ports; each warms up first and times only the loop, the same way the Dark script does.

Reproduce the other two with `scripts/testing/perf-workloads/crosslang/run`, and Dark with
`scripts/testing/perf-suite`.

---

## Allocation per iteration

This is the number the campaign steers by. It repeats to the byte, it doesn't care how loaded the
box is, and all three runtimes are made to report the same thing: bytes allocated by the work,
cumulatively, whether or not they were later freed. Getting there needs a different trick per
runtime, so the method is written next to each.

- **Dark** reports it directly, from `GC.GetTotalAllocatedBytes`. Startup is excluded by running the
  same script at 0 iterations and differencing.
- **node** gets a 512 MB young generation so no collection happens during the measured section,
  which makes `heapUsed` growth equal to everything allocated.
- **python** frees by refcount as it goes, so heap growth can't show it. Each iteration's result is
  retained instead, and `tracemalloc`'s traced total is what the work allocated. Transient wrappers
  (`range`, `map`) are still freed and go uncounted, worth a few hundred bytes an iteration.

```
  Dark        21.95 KB     ( 1,201 KB at the start of the second campaign, 90.8 KB a week ago )
  node         1.70 KB
  python       0.47 KB
```

**Dark is 12.9x node and 47x python.** It was 700x and 2,600x at the start of the second campaign,
and 53x and 193x a week ago. The goal is ~10x node, so there is about another 1.3x to find -- and
it is already measured: pooling the builtin argument buffers takes this workload to 11.9 KB, which
is 7x node. See task #68.

## Where that came from

Almost entirely from replacing representations rather than shaving call sites. In rough order of
what each was worth: the interpreter loop off Ply and onto F#'s resumable-code `task`; the type
symbol table from an `FSharpMap` to a struct; the ValueType of a container memoized instead of
rebuilt three times per call; the builtins from an `FSharpMap` to a `Dictionary`; the builtin
calling convention from a reference tuple to a struct one; frames and register files pooled; small
integers and booleans interned; record and enum field checking rewritten to stop allocating a
closure, a state machine and a tuple per field; builtin arguments passed as an array rather than a
list; package lookups holding the `Option` they hand back rather than rebuilding it.

Roughly as much was learned from the changes that measured flat and were reverted. Those are in the
`win-*.md` notes; the recurring lesson is to probe before building, by stubbing the suspect region
out to a constant and seeing whether the number actually moves.

## Timing

```
  node          0.93 ms
  python        0.88 ms
  bash            49 ms
  Dark           350 ms   (Debug; 384 ms at the start of the second campaign, 1,162 before the first)
  Dark          ~260 ms   (Release, which is what ships)
```

Timing has barely moved while allocation came down 43x, which is worth being honest about: this
campaign has been an allocation campaign. The interpreter is still walking a bytecode array in
managed code and type-checking as it goes, and none of that got fundamentally cheaper.

## One workload is not enough

Everything above is one shape of program: lists, lambdas and self-recursion. `scripts/testing/perf-suite`
runs six, and the reference workload turns out to be nearly the *cheapest*:

```
  recursion    69.27 KB per iteration      json         31.43 KB
  records      36.50 KB                    dicts        25.76 KB
  strings      21.77 KB                    lists        21.95 KB
```

A plain function call with no containers still costs three times a list iteration, though it was
five times before the argument-array change. And `scripts/testing/perf-http` says an HTTP request
that returns a constant string costs **197 KB**, nine times a list iteration, with the type checker
dominating its profile because requests and responses are records.

So the 12.9x above is the best case, not the typical one. The honest summary is that Dark is within
about 13x of node on the shape of program it has been tuned for, and considerably further away on
the shapes it hasn't -- though the gap between shapes is closing, because the last few wins came
from paths every program uses rather than from the list path.

## What's left, and what won't change

Two things Dark does that node and python don't: it type-checks every argument of every call at
runtime, and it can trace. Neither goes to zero while the language keeps the semantics it wants.

That said, most of what the type checker was spending was not the checking. It was rebuilding
answers it already had: resolved declarations, ValueTypes of containers, type-argument lists with
nothing in them. Caching those is most of what the last few days bought, and the profile suggests
there is more of the same left.
