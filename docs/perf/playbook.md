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
is. Time on this box drifts about 12 ms, enough to hide or invent most individual wins; eleven runs
cannot reliably resolve a 10 ms difference.

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
worse. Both checked rather than assumed. So several samples a side is the only thing that helps: one
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

The two most-used types in the language cost 4-6x anything else, which produced three commits. That
table is now `scripts/perf/workloads/costs.dark`; extend it rather than reinventing it, and note its
harness floor (~200 B) when reading rows.

## 4. Distrust your own counters

**Averaging two populations.** A counter reading "20 bytes on each of 41,402 calls" was really ~15 KB
on each of 52 *cold* calls, the rest free. Three changes were built against it, all flat. Stage
counters report run counts alongside bytes for this reason: always look at the denominator, and if a
number is suspicious, count the population.

**Brackets that span an await.** A bracket around a region containing a bind measures whatever
nested execution resumes inside it, not the region -- it can report more bytes than the process
allocated in total. Only bracket synchronous stretches.

A useful control: bracket *nothing*, next to the suspicious one. Non-zero means the instrument is
wrong rather than the code.

**One workload, one run, one instrument.** A figure assembled from parts measured in different
places is not a measurement. This round produced a phantom 1,240-byte layer by taking a function's
cost from one workload and the enclosing opcode's from another -- the same function costs 680 bytes
in one and 1,960 in the other. Bracketed on a single workload the numbers closed exactly.

**Never promote a residual.** A number you got by subtracting measured regions from a measured
total is not a measurement; it is everything you failed to bracket, plus your arithmetic. Four
suspects were promoted that way in one night -- each looked like thousands of bytes as a residual
and measured in the tens when bracketed directly. If the interesting number is the part you did not
measure, go and measure it. The two allocation counters, incidentally, do agree:
`GetAllocatedBytesForCurrentThread()` and `GetTotalAllocatedBytes(true)` returned identical figures
on a continuation-heavy path, so a thread hop is not the explanation for a gap.

**Not across two scripts either.** The night after "never promote a residual" went into this file,
a 70%-of-the-cost item was opened by subtracting a bracket taken in one probe script from a row
total taken in another. It survived one tick. Each number was measured honestly, which is what makes
this half easy to miss.

**Bound the outermost suspect first.** Five hypotheses about JSON's `convert` died in a row, every
one assuming the function was where the bytes were. A bracket around `Json.parse` itself settled it
in one build: the whole function is a third of what the call costs. One measurement either
implicates a function or clears it, and clearing it retires every hypothesis about its insides.

**Confirm a row measures what its label says.** Two rows labelled with two different types spent
three ticks looking like a 2x property. They were sitting on a bug where a type annotation resolves
to the wrong type, so the labels lied. Printing the parsed value once would have cost one run.
Related: type names are content hashes, so a probe's `type RBool = { a: Bool }` is the same type as
anything else with that shape. When one row of a sweep is an outlier, suspect the harness first.

**Read instruction counts next to byte counts.** They disagree with a wrong story faster. The
per-opcode counters caught one row taking an extra match arm, and separately proved that forwarding
hops really were executing while allocating nothing. Both results came from the denominator, not
the bytes.

**An intervention that shows nothing may not have intervened.** "The last-declared type is cheap"
was dismissed by adding a trailing type and seeing no change, but the type was unused and unused
declarations are dropped. Making it used reproduced the effect immediately.

**The first row of a sweep pays one-time cost.** Measure the baseline again at the end; a first row
has read 931 bytes where every later row read 231.

**A tight spread within one sweep is not low variance.** Three readings from one build can sit
inside 0.5% of each other and still be a percent or two away from three readings of the *next*
build, because each build reshuffles code layout. This misread twice in one campaign: a workload
looked like it regressed 1.2%, the next pass showed it below where it had started, and the pass
after that it was "up" again. Treat a sub-2% move across builds as unresolved until a third build
agrees, and let the most realistic workload decide a mixed result.

**Count the hit rate before explaining a disappointing fast path.** A fast path that never fires and
a fast path that fires but had little to remove produce the same small number, and they need
opposite fixes. The sync-build path on the record opcodes looked like the first and was the second:
400 hits, 0 misses. `byOpcode` in `interpreterStatsGet` reports `syncHit`/`syncMiss` for this.

## 5. Change one thing, measure, keep or revert

Revert anything flat, and say so. Not tidiness: a flat change is a diff someone reads forever, and
several together make the next bisect impossible.

Reverted-flat changes are worth about as much as landed ones, because they say where the cost
*isn't*. Write them down in the roadmap's closed section so nobody re-opens them.

## 6. Pick the workload deliberately, and re-pick it

Two campaigns took every number from one script. When six workloads finally existed, that script was
the second-cheapest of them and three of the round's biggest wins were invisible on it.

Run `scripts/perf/suite` (six shapes) and `scripts/perf/http` (the async path, under concurrency)
after anything structural. A change that helps one shape can cost another, and you cannot see that
from inside one workload.

Signs it's time to re-pick: returns are diminishing, the profile is flat, and the wins are getting
more specific to the thing you're measuring.

## 7. Verify past the test suite, by hand

The backend suite has **twice** passed while the interpreter returned a quietly wrong answer. For
anything touching execution run `scripts/perf/checks` and read the output.

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
- **Extract helpers as top-level functions and pass everything explicitly.** A rewrite meant to
  remove allocation added it instead, because the new helper closed over an array rather than taking
  it as a parameter -- and became the largest single entry in the profile.

## 9. Debug timings are not small-multiple wrong, they are order-of-magnitude wrong

Allocation is nearly identical between Debug and Release, which makes it tempting to trust Debug
*timings* too. Don't. On one `dark eval`, SQL measured 39.9 ms in Debug and 2.7 ms on the shipped
AOT binary, and deserialization 17.5 ms against 0.6 ms. A roadmap item survived for a while on the
Debug figure and evaporated when measured properly.

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
of the A/B you are about to run. It happened in the typecheck allocation work: the main baseline
binary was silently replaced by the changed one. Commit, even to a scratch branch, before every
publish, so each binary's name is a tree you can name back.

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
  identifier. CI publishes the solution with no `-r`, so it lands in `net10.0/publish/`;
  `build-release-cli-exes.sh` publishes per-RID into `net10.0/<rid>/publish/` and then *moves* it to
  `clis/`, emptying that. A tool that knows one layout passes in one environment and fails in
  another. `scripts/perf/_common` knows all three; use it rather than writing the paths again.
- `backend/Build/out` is a container volume and is not writable from the host. Staging anything there
  has to happen inside the container.
- Debug builds in ~2 minutes, Release in ~10. Develop in Debug, decide in Release, and know the two
  disagree by up to 3x on paths still heavy in computation expressions.

## 12. Leave the next person a gate, not a story

`scripts/perf/gate` asserts allocation against a checked-in budget, separately for Debug and
published, and CI runs it. **Lower the budget in the same commit that earns it** -- a budget nobody
tightens stops being a gate and becomes a ceiling to drift up to.

**Keep CI's share small.** The gate is deliberately one script and a couple of seconds. Don't put
`scripts/perf/suite` or `scripts/perf/http` in CI: the suite runs twelve processes, and `http` starts
a server and drives it under load. Those are for a human, or a nightly, deciding something. A long
perf job in the main pipeline gets ignored and then disabled.

Same for documents: one roadmap, one history, one playbook, updated in place. The alternative is
dozens of working notes and no way to tell which is current.

Same for workloads. Anything under `scripts/perf/workloads/` that builds a CLI view is written
against the views as they are today, so it falls behind the moment they change, and a file nothing
runs falls behind silently. Two rules keep that in check:

- **One workload per subject, not one per question.** `view.dark` reports time, allocation and the
  per-fn profile from a single setup, because three files sharing a copy of that setup is three
  things to fix when `viewAtSize` changes. Same for `route.dark`.
- **Anything that asserts goes in `workloads/checks/`, wired into `scripts/perf/checks`.** An
  assertion nobody runs is worse than no assertion: it reads like coverage and isn't. Everything at
  the top level is a hand instrument, and is expected to be re-pointed at whatever the code looks
  like when someone next needs it.

A probe written for one investigation is not an asset. Read what it told you into `history.md` or
the roadmap, and delete it; the next person's probe should be written against the code they have.

## An A/B is only an A/B if both arms run in the same session

The store drifts, the box gets busy, and a baseline taken an hour ago is not a baseline. A change once
read as a 4% regression against an earlier number and was flat when both arms were re-measured back to
back. Rebuild both arms and measure them together, always.

The same rule caught a per-call figure that was wrong by 20x, and an HTTP throughput claim that had to
be retracted -- `scripts/perf/http`'s throughput column is not comparable across sessions at all,
though its allocation column repeats to 0.1 KB.

## Instruments lie in specific ways

- `view.dark`'s profile section enables stats, and the builtin path then reads the allocation
  counter twice per call. Per-call figures from it are upper bounds; take absolute numbers from
  `optime`, which runs with stats off.
- `fnprofile` reports the minimum per call across however many runs you feed it. Feed it three.
  One run of it once misread a builtin by 20x.
- A profiler that keeps one call stack per VM double-counts anything run through `guard`, because
  `guard` runs its callback on a fresh VM (`executeApplicable1`): the caller is charged its
  callee's bytes as well as the callee. One such profile put the checker's loading at 1.6 GB when
  it was ~0.95. Charge per thread, to whatever frame is running when the bytes are allocated, keep
  the profiler's own bookkeeping out of the window, and before using any figure check that
  charged plus unaccounted sums to the total the same binary allocates with the profiler off. Also
  prove it on a probe of known size called both directly and through `guard`.
- `scripts/perf/suite`'s allocation is **not** byte-deterministic, unlike the gate's. A couple of
  percent there is noise.
- The wall-clock sections of `view.dark` and `route.dark` resolve about 1%; `keypress` reports whole
  milliseconds and cannot see a sub-millisecond win at all.

- `dark run` exits 0 when the script fails, and a failing run is fast. `bench` now reads the
  output for "Script error" and refuses the pair, but anything else that times a run has to
  look. The case that found it: an older binary could not read a policy file a newer one had
  written (a new effect name; missing or corrupt policy fails closed), so every A run was denied
  its clock call at once, and the new binary read as 4% slower on three workloads, 0/15 pairs.
  Two things to check before believing an A/B between binaries of different ages: that both
  arms print the workload's own `elapsed_ms`, and that `rundir/policy` was written by a binary
  the older arm understands.
---

---

## The gate could not reproduce its own baseline across a day

On 2026-10-02 the published gate read 17% over budget. It was not a regression. Checking out
`7973a64262`, the commit that had PINNED the budget the previous evening, building it and running
the debug gate on a clean store gave:

    steady.dark (debug) allocated 11.9 MB; budget 9.7 MB -- 23.0% over

That commit's own message records the same gate at 5.8% over, which is 10.2 MB. Same source tree,
same machine, eighteen hours apart, 1.7 MB different. There was nothing to bisect: the regression
was already present at the baseline that defined "no regression".

So a budget pinned from a reading taken on a working clone is pinned to that clone's afternoon.
**Re-pin only from CI**, where the environment is constructed rather than accumulated, and treat a
local red as a question rather than a finding until it reproduces somewhere clean.

Ruled out that day, each with a measurement rather than an argument:

- store dirt. Three store states, same binary: dirty 152 MB gave 11.8 MB, a copy of `seed.db`
  108 MB gave 11.4 MB, a clean 56 MB store gave 11.3 MB
- `rundir/policy/policies.bin`. Moving it aside did not lower the number; it made the gate exit 1
  with no output at all, because `initialized` without `policies.bin` fails closed
- a stored `trace.record` defeating the gate's `DARK_CONFIG_TRACE_DETAIL=off`. It was off. Worth
  knowing anyway: `config/dev` sets that variable to `on` container-wide, and a STORED setting
  beats the environment by design, so the gate's explicit `off` is load-bearing

Not ruled out, and left alone deliberately: the `backend/Build` volume carrying something between
builds, and the container's own baked environment (a container made from a sibling clone bakes
that branch's `config/dev`, and this one's `TRACE_DETAIL` default already disagreed with the file).

### 2026-10-03, settled: the gate reads 2.1 MB high on a dev store, and HOME is not why

Four readings, same binary (AOT, published from the same commit) and same workload, varying one
thing at a time. This is the measurement the entry above was missing.

    store                         HOME     recording   allocated   verdict
    dev rundir                    real     on (stored) 11.8 MB     22.1% over
    dev rundir                    fresh    on (stored) 11.8 MB     21.7% over
    dev rundir                    real     off         11.4 MB     17.6% over
    grown by the published binary fresh    off          9.7 MB     at budget, exit 0

So the 2.1 MB splits three ways, and only one of them was what anybody suspected:

- **HOME: nothing.** 0.0 MB, rows one and two. The suspicion was `capabilities.bin`, which is
  keyed on HOME and whose absence makes the host permissive. It was never there to matter:
  `/home/dark/.darklang/` is EMPTY in these containers, so both runs were already permissive.
  Worth knowing before anyone spends another experiment on it.
- **The store's stored `trace.record`: 0.4 MB.** Real, and a gate bug rather than a fact about
  stores. `gate` set `DARK_CONFIG_TRACE_DETAIL=off` and the ladder in `LibDB/Tracing.fs` puts the
  STORED setting above the environment, so on any store where somebody ran `dark traces record on`
  the gate was measuring a recording run. Proven directly: with `TRACE_DETAIL=off` set, one
  `eval 1L` against the dev store took `traces` from 71 to 72; with `--no-trace` it stayed at 72.
  `gate` passes `--no-trace` now, which pins the setting and beats both.
- **Store provenance: 1.7 MB, and the mechanism is NOT identified.** A store grown by the
  published binary from its own embedded seed reads 9.7; any store this dev tree produced reads
  11.3 to 11.4 with recording off. It is not size (150 MB fresh against 152 MB dev), not the op
  count (13,737 against 13,739), not dirt, and not config (the fresh store has zero `config_v0`
  rows and the dev store has exactly one, the `trace.record` already accounted for above). The
  three-way sweep in the entry above bottoming out at 11.3 is the same wall: all three of its
  stores were dev-derived, so none of them could get under it.

**CI is in the last row.** It runs `scripts/perf/gate --published` in a fresh container, against a
store the published binary seeds itself, with no stored `trace.record`. So the budget is reachable,
CI has been measuring the right thing, and it is the LOCAL readings that have been inflated. Two
separate conclusions and both are needed: the branch has not regressed allocation, and the local
instrument was wrong by a fifth of the budget.

What to do with a local red, in order: pass `--no-trace` (the gate does now), then re-run against a
store the published binary grows itself, and only then start bisecting.

---

## The store you measure against is half the measurement

Four lessons that each cost a measurement. None of them is about a particular change; they are
about the thing every reading here is taken against.

### A store you copied is a store from a point in time, and the time is before your change

The general rule behind the next section, and the one that has now cost three measurements in a
day. A store is a snapshot: copying one, exporting one, or reloading one pins the code it
contains. Measure after a change against a store taken before it and the change reads as having
done nothing.

- a seed exported mid-session, measured later as if it were clean (the next section)
- a throwaway rundir seeded with `_copy-store` BEFORE an edit, then used to test the edit. The fix
  was live in the dev store and absent from the copy, so the first "after" run reproduced the
  bug exactly
- the dev store itself, after `git checkout <base>` ran a build that reloaded BASE packages into
  it. Coming back to the branch does not undo that, and a plain build then says "nothing has
  changed", so the tree had the fix and the store did not. The measurement that exposed it was a
  gate taking 129s instead of 12s, because the old code was still being killed by its timeout

The check is the same in all three: before trusting a number, ask what the store was built from and
WHEN. `scripts/dev/build` after returning from a detached checkout, and re-copy any throwaway store
after a change you intend to measure.

### The seed is not automatically clean

"Build a fresh store from `rundir/seed.db`" is only valid if nobody has re-exported the seed since.
`export-seed` writes it FROM the live store, so a seed exported during a working session carries
that session's ops. Mine had the same day's mtime and gave 11.4 MB, which is exactly the dirty
figure, and it nearly got written up as dirt a second time.

**Look at `ls -la rundir/seed.db` before trusting a store built from it.** To get a store that is
genuinely clean, delete `rundir/data.db*` and run `scripts/build/reload-packages`, which authors
packages from source and carries no traces, no hand-authored modules and no approvals.

Size is the other tell, and it is blunt enough to use without thinking. A freshly exported seed on
this branch is 13 MB. The one that had been sitting in `rundir/` was 108 MB, so it was carrying
about 95 MB of one session's ops. If `seed.db` is an order of magnitude larger than a fresh export,
every "fresh store" built from it was a working store wearing a fresh store's name.

A build will eventually tell you the seed is stale, but only when the package refs move:
`rundir/seed.db cannot produce this binary's package refs`. Nothing tells you it is merely dirty.

### Dirt is worth about 4%, not 19%

An earlier commit message asserted that nine days of accumulated ops inflated the gate by about
19%. The three-way measurement above says about 4%, and the 19% was itself an artifact of
comparing against a seed that was not clean. This matters because that figure had become the
standard reason to dismiss a bad reading, and it was wrong in the direction that made dismissal
too easy.

### A number far UNDER budget deserves the same suspicion as one far over

Attempting an A/B against an older published binary gave 5.1 MB against a 9.7 MB budget, a
plausible-looking 2.2x win. It was a failed run: that binary could not read the newer store
("Function ... couldn't be found"), and stderr had gone to `/dev/null`. The only thing that caught
it was the number being implausibly GOOD.

This is a repeat, and the guard was already written down twice. The section above already says to
check that both arms print the workload's own `elapsed_ms`, and `rundir/alloc-bisect.sh` exists for
exactly this job: its header notes that twice on 30 September a binary that was not doing the work
produced a plausible number, so it refuses a run whose summary line is absent. Use that script
rather than writing a fresh loop, which is how this was hit again.

Counting the ones we know about: two on 30 September, two the evening of 1 October, and the 5.1 MB
above. **Five plausible wrong readings from this instrument.** The count is the argument: the gate
is not a reliable instrument on a working clone, and the budget is pinned from readings taken
with it.
