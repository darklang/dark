# scripts/perf

Everything here measures the runtime. This file is the map: what each tool is, which workload it
runs, and the recipe for a round of perf work. The writing lives in `docs/perf/`: `playbook.md` is
how to do this work and the traps, `roadmap.md` is what is worth doing next, `history.md` is the
numbers round by round.

## The one rule

**Compare allocation, and only compare AOT against AOT.** Allocation for a fixed workload repeats to
the byte and does not care how loaded the box is. Wall time on a dev box drifts ~30% between passes
on the same binary, Debug and Release disagree by up to 3x, and AOT and R2R by ~5x on startup. Every
number that goes in a PR, a document or a message comes from an AOT Release binary; Debug is for
iterating.

`build-release-cli-exes.sh` writes the mode to `clis/<binary>.mode`. Read it rather than assuming.

## Asking what one thing costs

Before reaching for any of the tools below: `Darklang.LanguageTools.Perf.measure` takes a thunk and
gives back allocation, time and the interpreter's counters, with whatever the thunk returned.

    let harness = Perf.floor ()
    let (raw, answer) = Perf.measure (fun () -> Stdlib.List.map input inc)
    Perf.render (Perf.minus raw harness)

`floor` is what `measure` costs when the thunk does nothing; subtract it or small readings are
mostly harness. One primitive on purpose: a function against its replacement, one function across
five input sizes, a survey of twenty builtins, are all loops over it, which is cheaper than
curating input sets per function.

From the command line, `dark eval --stats <expr>` prints the same block. Note what it measures:
evaluating an expression parses and resolves it too, and for anything small that is most of the
allocation, while the instruction count is execution only. Inside a script, `Perf.measure` brackets
the work alone.

Every measurement carries the build that produced it (`debug`, `release-r2r`, `release-aot`),
because the same workload has three different right answers.

## The recipe

1. `scripts/dev/build` and get the thing working first.
2. `scripts/perf/gate` for a quick debug reading (it reports, it does not gate).
3. Pick a workload that is the shape you are changing, from `workloads/` below.
4. Probe with `alloc-profile` (what type is eating the heap) or `fnprofile` (which fn the time is
   in). Once nothing is above ~5%, stop profiling and build a before-and-after instead.
5. Publish AOT: `scripts/run-local-exec export-seed rundir/seed.db` then
   `scripts/build/build-release-cli-exes.sh --mode=aot`.
6. `scripts/perf/gate --published` for the budgeted number, `scripts/perf/suite --release` for the
   seven shapes, `scripts/perf/bench ab <a> <b> <scenario>` to compare two binaries.
7. If the change earns a lower budget, lower it in the same commit:
   `scripts/perf/gate --published --update`.
8. Publish, THEN run the gates. `scripts/testing/gates first-day` refuses an artifact older than
   the tree, which reads as a failure if you do it the other way round.

## The tools

- `gate` is the CI assertion: `workloads/steady.dark`, allocation against `budget.json`.
  `--published` is the real one; bare is debug and does not gate.
- `suite` runs the seven shapes in `workloads/suite/`, allocation and time per iteration, with
  startup differenced out.
- `bench` (and `_bench.py`) is repeatable CLI timing from a pinned store fixture. `bench ab`
  interleaves two binaries and reports the paired median, which is drift-proof.
- `http` is allocation and latency per request under concurrency: the only tool here that sees the
  async half of the runtime.
- `keystroke` is one keypress in the interactive CLI, end to end. Needs a TTY, so it drives
  `expect`.
- `first-sync` is a whole first sync from an empty home, which is the number a new person waits.
- `alloc-profile` is allocation by type name, from GC allocation-tick events.
- `fnprofile` ranks per-package-fn time and resolves hashes to names. Feed it three runs: it takes
  the minimum per call, and one run of it once misread a function by 20x.
- `crosslang` runs the reference workload in node and python, for scale.
- `view-telemetry.py` shows where a run's time went, from `rundir/logs/telemetry.jsonl`.
- `_common` holds the shared shell helpers: where a published binary is, which mode it is, whether
  it is stale.
- `_total-alloc.py` is the one parser for "bytes this run allocated", used by `gate` and `suite`.

## The workloads

Three different contracts, which is why they are in three places:

- `workloads/steady.dark` and `workloads/http-server.dark` are owned by a tool. `steady.dark` is the
  gated workload, named in `budget.json`, so changing it changes what the budget means. Do not edit
  it to make a number look better.
- `workloads/suite/` is the differenced set. Each file reads `PERF_ITERS` and gets run twice, at 0
  and at N, so startup cancels and the result is per-iteration. A new file here is picked up
  automatically.
- `workloads/bench/` is staged into `rundir/perf-workloads/` by `bench`. These use raw `Builtin.`
  calls rather than the Dark wrappers on purpose: an A/B runs the same source against an older
  published binary, whose store may not carry today's wrapper.
- `workloads/probes/` is hand-run. One file per question (what a view costs, what a lambda
  application costs, where routing time goes, where spreading starts to pay). Run them with
  `./scripts/run-cli run <file>` and read the output; each file's header says what it answers and
  how to read it. The `perf-probes-run` gate sets `DARK_PERF_SMOKE=1` to check every probe with
  fewer repetitions. It keeps every workload row, list size, canvas size and workbench view;
  the operation probes use inputs on both sides of their conditional examples. It also runs
  the allocation and profiling sections. Smoke output is labelled and must not be used as a
  performance measurement. With the variable unset, manual runs keep their full repetitions.
  The gate reports elapsed seconds per probe to help identify slow checks. The separate AOT
  allocation gate and its budget are unchanged.

## What is NOT here

Correctness checks. There used to be a `checks` script and a `workloads/checks/` directory holding
golden-output comparisons for the interpreter, operator fast paths, SGR parsing, grapheme handling
and two rendered views. Those are all package tests now, in `packages/darklang/tests/`, where they
are asserted by `dark test run` rather than read by eye. If you want to check that a perf change
kept behaviour identical, run the package tests.

## Reading a number without fooling yourself

The playbook has the long version. The four that cost the most time here:

- **A published binary older than the tree measures the previous build.** `gate` and
  `gates first-day` refuse one rather than reporting; believe them.
- **The store is half the measurement.** The same binary reads about 2 MB higher against a working
  dev store than against one the published binary grew itself. CI is in the second case.
- **A zero-second run is a lock, not a result**, and a suspiciously good number usually means the
  thing you think produced it did not run.
- **`gate` measures the whole process**, so startup, opening the store, loading packages and
  parsing the file are all inside its number. Measured on an AOT binary with a warm store:
  `Stdlib.printLine "baseline"` allocates 6.6 MB and all of `steady.dark` allocates 10.3 MB, so
  about two thirds of the budget is not the loop under test. `suite` differences two runs and does
  not have this problem.
