# crosslang

The same small programs in Dark, python, node and bash, and a driver that times them side by side.
It's for scale ("how far is Dark from python on a list loop"), not a gate: nothing in CI runs it.

One directory per scenario, `workload.dark` / `.py` / `.js` / `.sh` inside it:

    hello            startup and one print
    steady           the shape of workloads/steady.dark, the gated workload
    lists            build, map, filter, length, in a loop
    strings          build, join, split back
    dict             set and get
    records          build and read records
    recursion-deep   recursion 5000 deep, 50 times
    recursion-fib    fib 24
    arith            an integer loop
    sort             sort 50,000 ints
    jsonrt           serialize and parse back
    fileread         count matching lines in a 100,000-line file

`harness.py` and `harness.js` wrap the python and node programs so they can report allocation.
`driver.py` runs everything.

---

## Running it

Inside the container, against a published AOT binary (a Debug or plain Release CLI gives numbers
that mean nothing here):

    scripts/run-local-exec export-seed rundir/seed.db
    scripts/build/build-release-cli-exes.sh --mode=aot
    cat clis/darklang-alpha-*-linux-x64.mode
    DARK_EXE=clis/darklang-alpha-<sha>-linux-x64 \
      python3 scripts/perf/crosslang/driver.py --rounds 7 --alloc --out rundir/crosslang.json

`--only lists sort` runs a subset. Without `--out` the JSON goes to stdout, and either way a table
follows it.

The driver writes the `fileread` fixture to `rundir/crosslang-lines.txt` and grants the Dark binary
read access to that one path, since the instance policy denies file reads by default (without the
grant the Dark row exits 1 in 40 ms, which looks like a very fast pass).

---

## Reading it

**Time.** Each cell is `min / max` ms over the rounds, for the whole process, startup included. The
languages run round robin rather than one at a time, so drift on the box doesn't land on whichever
went last. Read the min; a run can be slowed by the machine but never sped up by it. A wide spread is
worth a look on its own. Load average is recorded per round in the JSON.

**Allocation** (`--alloc` only, one run each). These columns are NOT comparable across a row:

- Dark (marked `*`) is everything the process allocated, from the CLI's `gc.stats` telemetry, so it
  carries startup and package loading.
- node is the `heapUsed` delta across the work alone, with GC held off so the delta is everything
  allocated.
- python is the change in live traced memory across the work. CPython frees by refcount as it goes,
  so for a program that returns a number this is about zero however much it churned. The JSON also
  has `peak_bytes`, the high-water mark, which is the more useful python figure and still means
  "most alive at once", not "total allocated".
- bash reports nothing.

So read a column down the commits, not a row across the languages. For a Dark per-iteration figure
that sits beside python and node fairly, use `scripts/perf/suite lists`, which differences startup
out.

**Correctness.** Every program prints `result=<v>` and the driver checks all languages agree, so it
can't quietly time a program that crashed. Disagreements print as `MISMATCHED RESULTS`, non-zero
exits as `FAILURES`. Neither stops the run, but either makes the driver exit 1.

**Gaps.** Three bash rows are excluded in the driver, with the reason next to each: `records` (no
record type), `jsonrt` (no JSON; jq would be a different program) and `recursion-deep` (bash
segfaults at depth 5000; it manages 4000). The `fileread` bash row is mostly grep, a separate C
program, and the comment in it says so.
