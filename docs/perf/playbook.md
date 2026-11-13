# How to do performance work on Dark

Method, not results. The numbers live in `docs/perf/history.md`, what to do next in
`docs/perf/roadmap.md`.

Picking this up cold: read this, then the roadmap, then run `scripts/perf/suite` to see where things
actually are before believing anything.

The one-line version: **nearly all wasted effort came from trusting a measurement nobody had
checked.** Everything below is a way of not doing that.

---

## 1. Measure allocation, not time

Allocation for a fixed workload is far steadier than time and doesn't care how loaded the machine
is. Time drifts enough to hide or invent most individual wins.

It is not, however, byte-identical. `steady.dark` in debug, ten runs back to back:

    mean    7,343,612 bytes
    stdev      15,969   = 0.22%
    spread     56,504   = 0.77%

So treat ~0.8% as the noise floor for a single run of this workload, and do not believe a win
smaller than that from one measurement each side.

**The store counts as part of the workload.** The same binary on a working rundir allocates about
2.5% more than on a freshly loaded one, measured both ways: enough to swamp the noise floor and
enough to fail the gate. `scripts/perf/gate` prints which store it measured for this reason. A
number is only comparable to another taken against the same store.

The readings cluster, too: within a cluster they agree to 0.0003%-0.03%, and the clusters sit
0.3%-1.0% apart, so a single reading tells you which cluster you landed in rather than what the code
allocates. What picks the cluster is unknown, and it is not the shared dev store -- an isolated copy
of `data.db` clusters the same way, so don't spend time isolating the store again.

Taking the minimum of N does not tighten the readings much, since the spread is roughly symmetric
rather than noise sitting above a floor, and `GC.GetTotalAllocatedBytes(precise: true)` is slightly
worse. So several samples a side is the only thing that helps: one
reading is not a measurement, and never `--update` off a single sample.

Decide with allocation, report time, never gate on time. A change that looks flat in allocation and
good in time is flat.

The consequence, worth stating plainly: allocation campaigns move allocation. Time follows, but far
less. If wall-clock is the goal, that is a different campaign with a different instrument.

## 2. Probe before you build

Before writing a fix, **stub the suspect region out to a constant** -- semantically wrong, ten
minutes -- and see whether the number moves. It gives you the ceiling before you spend the day, and
probes have tracked the real implementation closely enough to trust for go/no-go.

Almost every change that measured flat and was reverted was built without probing first.

## 3. When the profile goes flat, build a comparison instead

`scripts/perf/alloc-profile` is excellent until it isn't. Early on one entry is 30-50% and the work
is obvious. Later nothing is above 5%, and "no single thing dominates" is true and useless.

`alloc-profile` also has a floor: it samples one tick per ~100 KB, so a workload that allocates only
a few megabytes yields a few dozen ticks and resolves nothing. Amplify by looping the work in-process
first -- and if the work is cached per process and therefore cannot be looped, the profiler is simply
the wrong instrument for it.

At that point stop profiling and **put comparable things side by side**. Measuring return types
against each other was one line of insight when the json profile was flat:

    returns Int                   221 B
    returns List<Int>             671 B
    returns a record            1,338 B
    returns Option<Int>         4,162 B
    returns Result<Int,String>  5,202 B

The two most-used types in the language cost 4-6x anything else. That table is now
`scripts/perf/workloads/probes/costs.dark`; extend it rather than reinventing it, and note its
harness floor (~200 B) when reading rows.

## 4. Distrust your own counters

**Averaging two populations.** A counter reading "20 bytes on each of 41,402 calls" was really ~15 KB
on each of 52 *cold* calls, the rest free. Stage counters report run counts alongside bytes for this reason: always look at the denominator, and if a
number is suspicious, count the population.

**Brackets that span an await.** A bracket around a region containing a bind measures whatever
nested execution resumes inside it, not the region -- it can report more bytes than the process
allocated in total. Only bracket synchronous stretches.

A useful control: bracket *nothing*, next to the suspicious one. Non-zero means the instrument is
wrong rather than the code.

**One workload, one run, one instrument.** A figure assembled from parts measured in different
places is not a measurement: the same function can cost 680 bytes in one workload and 1,960 in
another, so taking a function's cost from one and the enclosing opcode's from another invents a
layer. Bracket on a single workload.

**Never promote a residual.** A number you got by subtracting measured regions from a measured
total is not a measurement; it is everything you failed to bracket, plus your arithmetic. A
residual of thousands of bytes can measure in the tens when bracketed directly. If the interesting number is the part you did not
measure, go and measure it. The two allocation counters, incidentally, do agree:
`GetAllocatedBytesForCurrentThread()` and `GetTotalAllocatedBytes(true)` returned identical figures
on a continuation-heavy path, so a thread hop is not the explanation for a gap.

**Not across two scripts either.** Subtracting a bracket taken in one probe script from a row
total taken in another is the same residual. Each number is measured honestly, which is what makes
this half easy to miss.

**Bound the outermost suspect first.** Bracket the whole function before theorising about its
insides. One measurement either
implicates a function or clears it, and clearing it retires every hypothesis about its insides.

**Confirm a row measures what its label says.** A type annotation that resolves to the wrong type
makes a row's label lie, and printing the parsed value once costs one run.
Related: type names are content hashes, so a probe's `type RBool = { a: Bool }` is the same type as
anything else with that shape. When one row of a sweep is an outlier, suspect the harness first.

**Read instruction counts next to byte counts.** They disagree with a wrong story faster, and they
show work that allocates nothing, such as an extra match arm or a forwarding hop.

**An intervention that shows nothing may not have intervened.** Adding an unused type to a probe
changes nothing, because unused declarations are dropped; make it used.

**The first row of a sweep pays one-time cost.** Measure the baseline again at the end.

**A tight spread within one sweep is not low variance.** Three readings from one build can sit
inside 0.5% of each other and still be a percent or two away from three readings of the *next*
build, because each build reshuffles code layout. Treat a sub-2% move across builds as unresolved until a third build
agrees, and let the most realistic workload decide a mixed result.

**Count the hit rate before explaining a disappointing fast path.** A fast path that never fires and
a fast path that fires but had little to remove produce the same small number, and they need
opposite fixes. `byOpcode` in `interpreterStatsGet` reports `syncHit`/`syncMiss` for this.

## 5. Change one thing, measure, keep or revert

Revert anything flat, and say so. Not tidiness: a flat change is a diff someone reads forever, and
several together make the next bisect impossible.

Reverted-flat changes are worth about as much as landed ones, because they say where the cost
*isn't*. Write them down in the roadmap's closed section so nobody re-opens them.

## 6. Pick the workload deliberately, and re-pick it

One script can be one of the cheapest workloads and hide the biggest wins entirely.

Run `scripts/perf/suite` (six shapes) and `scripts/perf/http` (the async path, under concurrency)
after anything structural. A change that helps one shape can cost another, and you cannot see that
from inside one workload.

Signs it's time to re-pick: returns are diminishing, the profile is flat, and the wins are getting
more specific to the thing you're measuring.

## 7. Verify past the test suite, by hand

The backend suite can pass while the interpreter returns a quietly wrong answer. For
anything touching execution run the package tests (`dark test run`) and read the output.

Error paths deserve the most attention: they only run once something has already gone wrong, so a
change can garble every message with every test green.

Two traps when writing such a check:

- **Structurally identical types are the same type** (names are content hashes), so passing a `B`
  where an `A` is expected tests nothing. Make the shapes genuinely different.
- For behaviour-preservation work, **record the output before the change** and diff after.

And run the suite with tracing **on** as well as off when touching anything the tracer reads: it
reads a builtin's arguments *after* the body has returned, a lifetime nobody thinks about.

## 8. Refactoring a hot path without ending up with two of it

- **Don't share a local between the fast and slow branch.** F# can't lambda-lift a local a closure
  captures, so a value bound "just to save a line" allocates on every call to serve a branch that is
  never taken after the first. Spell the call out in both arms.
- **Make the moved-from arm raise, not silently do nothing.** A no-op left behind by a move produces
  a silent wrong answer; raising catches it on the first run.
- **Extract helpers as top-level functions and pass everything explicitly.** A helper that closes
  over an array rather than taking it as a parameter allocates, and can become the largest single
  entry in the profile.

## 9. Debug timings are not small-multiple wrong, they are order-of-magnitude wrong

Allocation is nearly identical between Debug and Release, which makes it tempting to trust Debug
*timings* too. Don't. SQL and deserialization each run more than ten times slower in Debug than on
the shipped AOT binary.

Profile allocation in Debug freely. Never quote a Debug duration, and never rank work by one.

And never mix runs. A percentage built from a numerator taken in one run and a denominator taken in
another is not a measurement. Startup here has ±20% run-to-run spread, so any split of it needs a
median over ten or more runs, all from the same configuration.

## 10. Know what your binary actually contains

A Release build takes ten minutes, so measuring against the one you built earlier is tempting. Don't.
`build-release-cli-exes.sh` names the binary after `git rev-parse HEAD` *at build time*, which is
worse than no label: it looks authoritative and is wrong if the tree had uncommitted changes or has
moved on. Before quoting a Release number, check the binary is newer than every commit you are
crediting.

The same naming destroys your baseline. Publish with uncommitted changes and the new binary lands
on the HEAD-named path, overwriting the binary of the committed tree, which is usually the "before"
of the A/B you are about to run, and nothing says it happened. Commit, even to a scratch branch,
before every publish, so each binary's name is a tree you can name back.

## 11. Working with the tooling here

- Run the perf tools in the **foreground**, with `< /dev/null`. Backgrounded shell commands are
  throttled here by ~50x, which reads as a regression.
- Pass `< /dev/null` to everything going through `scripts/run-in-docker`, or it hangs after finishing
  and every later command in the clone crawls. Never `pkill -f run-in-docker` -- it matches other
  clones and your own command line. Filter on `/proc/<pid>/cwd`.
- `scripts/dev/build` often doesn't exit after succeeding: redirect output and check
  `scripts/dev/status`.
- `alloc-profile` needs `--rundown false`, or the JIT rundown is most of the trace and the profile
  looks flat when it isn't.
- Environment variables do **not** survive the container re-entry these scripts do. Make it a flag.
- `ls a b` exits **non-zero when either operand is missing**, even though it prints the one that
  matched. Under `set -euo pipefail` that kills the script mid-assignment with no message.
- There are **three** places a published binary can be, and the difference is the runtime
  identifier. CI publishes the solution with no `-r`, so it lands in `net11.0/publish/`;
  `build-release-cli-exes.sh` publishes per-RID into `net11.0/<rid>/publish/` and then *moves* it to
  `clis/`, emptying that. A tool that knows one layout passes in one environment and fails in
  another. `scripts/perf/_common` knows all three; use it rather than writing the paths again.
- `backend/Build/out` is a container volume and is not writable from the host. Staging anything there
  has to happen inside the container.
- Debug builds in ~2 minutes, Release in ~10. Develop in Debug, decide in Release, and know the two
  disagree by up to 3x on paths still heavy in computation expressions.

## 12. Leave the next person a chart, not a story

`scripts/perf/gate` checks allocation, instructions and CPU against a checked-in budget, on the
published build only. Startup and the workload net of startup have separate budgets, so neither
dilutes the other.

**CI does not gate on any of it.** A budget in CI blocks merges on noise and on regressions
somebody else already landed, and pushes every perf-positive PR into rewriting budget.json. So
performance is tracked over releases instead: `scripts/perf/measure-release <tag>` takes every
number for one release binary, with this tree's workloads, and `scripts/perf/track` does that for
each release a history does not have yet, then charts the lot and prints it. Because the
instrument is fixed and the binaries are the shipped ones, old releases can be measured too, so a
history can be backfilled.

The budget is the baseline the charts draw. Move it with `scripts/perf/gate --published --update`,
in a commit of its own, once a change is real and wanted.

Same for documents: one roadmap, one history, one playbook, updated in place. The alternative is
dozens of working notes and no way to tell which is current.

Same for workloads. Anything under `scripts/perf/workloads/` that builds a CLI view is written
against the views as they are today, so it falls behind the moment they change, and a file nothing
runs falls behind silently. Two rules keep that in check:

- **One workload per subject, not one per question.** `view.dark` reports time, allocation and the
  per-fn profile from a single setup, because three files sharing a copy of that setup is three
  things to fix when `viewAtSize` changes. Same for `route.dark`.
- **Anything that asserts goes in `packages/darklang/tests/`, as a package test.** An
  assertion nobody runs is worse than no assertion: it reads like coverage and isn't. Everything at
  the top level is a hand instrument, and is expected to be re-pointed at whatever the code looks
  like when someone next needs it.

A probe written for one investigation is not an asset. Read what it told you into `history.md` or
the roadmap, and delete it; the next person's probe should be written against the code they have.

## An A/B is only an A/B if both arms run in the same session

The store drifts, the box gets busy, and a baseline taken an hour ago is not a baseline. Rebuild
both arms and measure them together, always.

`scripts/perf/http`'s throughput column is not comparable across sessions at all,
though its allocation column repeats to 0.1 KB.

## Instruments lie in specific ways

- `view.dark`'s profile section enables stats, and the builtin path then reads the allocation
  counter twice per call. Per-call figures from it are upper bounds; take absolute numbers from
  `optime`, which runs with stats off.
- `fnprofile` reports the minimum per call across however many runs you feed it. Feed it three.
- A profiler that keeps one call stack per VM double-counts anything run through `guard`, because
  `guard` runs its callback on a fresh VM (`executeApplicable1`): the caller is charged its
  callee's bytes as well as the callee. Charge per thread, to whatever frame is running when the bytes are allocated, keep
  the profiler's own bookkeeping out of the window, and before using any figure check that
  charged plus unaccounted sums to the total the same binary allocates with the profiler off. Also
  prove it on a probe of known size called both directly and through `guard`.
- `scripts/perf/suite`'s allocation is **not** byte-deterministic, unlike the gate's. A couple of
  percent there is noise.
- The wall-clock sections of `view.dark` and `route.dark` resolve about 1%; `keypress` reports whole
  milliseconds and cannot see a sub-millisecond win at all.

- `dark run` exits 0 when the script fails, and a failing run is fast. `bench` reads the
  output for "Script error" and refuses the pair, but anything else that times a run has to
  look. An older binary cannot read a policy file a newer one wrote with a new effect name
  (missing or corrupt policy fails closed), so its runs are denied at once and the newer binary
  reads as slower. Two things to check before believing an A/B between binaries of different ages: that both
  arms print the workload's own `elapsed_ms`, and that `rundir/policy` was written by a binary
  the older arm understands.
---

---

## The gate could not reproduce its own baseline across a day

A budget pinned from a reading taken on a working clone is pinned to that clone's state. **Re-pin
only from a fresh container**, against a store the published binary seeds itself, where the
environment is constructed rather than accumulated, and treat a local red as a question rather
than a finding until it reproduces somewhere clean.

Moving `rundir/policy/policies.bin` aside does not lower the number; it makes the gate exit 1 with
no output at all, because `initialized` without `policies.bin` fails closed.

### The gate reads high on a dev store, and HOME is not why

- **HOME: nothing.** `capabilities.bin` is keyed on HOME and its absence makes the host
  permissive, but `/home/dark/.darklang/` is EMPTY in these containers, so runs are permissive
  either way.
- **A stored `trace.record`.** The ladder in `LibDB/Tracing.fs` puts the STORED setting above the
  environment, and `config/dev` sets `DARK_CONFIG_TRACE_DETAIL=on` container-wide, so on any store
  where somebody ran `dark traces record on`, `DARK_CONFIG_TRACE_DETAIL=off` alone measures a
  recording run. `gate` passes `--no-trace`, which pins the setting and beats both.
- **Store provenance, and the mechanism is NOT identified.** A store grown by the published
  binary from its own embedded seed reads at budget; any store this dev tree produced reads about
  17% over with recording off. It is not size, not the op count, not dirt, and not config.

What to do with a local red, in order: pass `--no-trace` (the gate does), then re-run against a
store the published binary grows itself, and only then start bisecting.

---

## The store you measure against is half the measurement

Four lessons, none of them about a particular change: they are about the thing every reading here
is taken against.

### A store you copied is a store from a point in time, and the time is before your change

The general rule behind the next section. A store is a snapshot: copying one, exporting one, or reloading one pins the code it
contains. Measure after a change against a store taken before it and the change reads as having
done nothing.

- a seed exported mid-session, measured later as if it were clean (the next section)
- a throwaway rundir seeded with `_copy-store` BEFORE an edit, then used to test the edit. The fix
  was live in the dev store and absent from the copy, so the first "after" run reproduced the
  bug exactly
- the dev store itself, after `git checkout <base>` ran a build that reloaded BASE packages into
  it. Coming back to the branch does not undo that, and a plain build then says "nothing has
  changed", so the tree had the fix and the store did not

The check is the same in all three: before trusting a number, ask what the store was built from and
WHEN. `scripts/dev/build` after returning from a detached checkout, and re-copy any throwaway store
after a change you intend to measure.

### The seed is not automatically clean

"Build a fresh store from `rundir/seed.db`" is only valid if nobody has re-exported the seed since.
`export-seed` writes it FROM the live store, so a seed exported during a working session carries
that session's ops.

**Look at `ls -la rundir/seed.db` before trusting a store built from it.** To get a store that is
genuinely clean, delete `rundir/data.db*` and run `scripts/build/reload-packages`, which authors
packages from source and carries no traces, no hand-authored modules and no approvals.

Size is the other tell, and it is blunt enough to use without thinking. If `seed.db` is an order
of magnitude larger than a fresh export, every "fresh store" built from it is a working store
wearing a fresh store's name.

A build will eventually tell you the seed is stale, but only when the package refs move:
`rundir/seed.db cannot produce this binary's package refs`. Nothing tells you it is merely dirty.

### Dirt is worth about 4%, not 19%

Accumulated ops inflate the gate by about 4% (dirty, seed-copy and clean stores, same binary). A
figure of 19% came from comparing against a seed that was not clean. Do not use dirt to dismiss a
bad reading bigger than that.

### A number far UNDER budget deserves the same suspicion as one far over

A failed run reads as a win. An older published binary that cannot read the newer store
("Function ... couldn't be found"), with stderr sent to `/dev/null`, gives a plausible-looking 2x
under budget, and only the number being implausibly GOOD gives it away.

Check that both arms print the workload's own `elapsed_ms`, and refuse a run whose summary line is
absent. **The gate is not a reliable instrument on a working clone**, so do not pin a budget from
readings taken with it.
