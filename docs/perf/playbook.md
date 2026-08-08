# How to do performance work on Dark

Written across three campaigns. This is method, not results -- the numbers live in
`docs/perf/history.md` and what to do next lives in `docs/perf/roadmap.md`.

If you are an agent picking this up cold: read this file, then the roadmap, then run
`scripts/testing/perf-suite` to see where things actually are before believing anything.

The one-line version: **almost all the wasted effort, in every campaign, came from trusting a
measurement I hadn't checked.** Everything below is a way of not doing that.

---

## 1. Measure allocation, not time

Allocation for a fixed workload repeats **to the byte** and doesn't care how loaded the machine is.
Time on this box drifts about 12 ms, which is enough to hide or invent most individual wins. Eleven
runs cannot reliably resolve a 10 ms difference; five certainly can't.

So: decide with allocation, report time, never gate on time. When a change looks flat in allocation
and good in time, it's flat.

Both campaigns were allocation campaigns for this reason, and it's worth being honest that time
moved far less than allocation as a result (27x vs 1.4x in round 2). If wall-clock is the goal, that
needs a different campaign with a different instrument.

## 2. Probe before you build

The highest-leverage habit discovered. Before writing a fix, **stub the suspect region out to a
constant** -- semantically wrong, ten minutes -- and see whether the number moves.

It tells you the ceiling before you spend the day. For the argument buffer, an unsafe probe
predicted 11.9 KB and 0.35 KB per iteration; the real, safe implementation delivered 12.0 and 0.40.
For container `ValueType`s, stubbing them to a constant took three stage counters from 1.79 MB to
0.03 and said the target was real before a line of the fix existed.

The changes that measured flat and were reverted are, almost without exception, ones I built without
probing first.

## 3. When the profile goes flat, build a comparison instead

The type profiler (`scripts/testing/alloc-profile`) is excellent until it isn't. Early on, one entry
is 30-50% and the work is obvious. Later, nothing is above 5% and the profiler says "no single thing
dominates" -- which is true and useless.

At that point stop profiling and **put comparable things side by side**. The json workload's profile
was flat; measuring return types against each other was one line of insight:

    returns Int                   221 B
    returns List<Int>             671 B
    returns a record            1,338 B
    returns Option<Int>         4,162 B
    returns Result<Int,String>  5,202 B

The two most-used types in the language cost 4-6x anything else. That produced three separate
commits. It's now `scripts/testing/perf-workloads/costs.dark`; extend it rather than reinventing it,
and note its harness floor (~200 B) when reading rows.

## 4. Distrust your own counters

Two specific failures, both of which cost days.

**Averaging two populations.** A stage counter read as "20 bytes on each of 41,402 calls". It was
really ~15 KB on each of 52 *cold* calls, with the other 41,350 free. Three changes were built
against that reading, all flat, before I instrumented the hit rate (45,534 hits, 10 misses). Stage
counters now report **run counts alongside bytes** for exactly this reason. Always look at the
denominator, and if a number is suspicious, count the population.

**Brackets that span an await.** An allocation bracket around a region containing a bind measures
whatever nested execution resumes inside it, not the region. This produced a counter claiming more
bytes than the process allocated in total. Only bracket synchronous stretches.

A useful control: put a bracket around *nothing*, adjacent to the suspicious one. If it reports
non-zero, the instrument is wrong rather than the code.

## 5. Change one thing, measure, keep or revert

Revert anything flat, and say so. This is not tidiness -- a flat change is a diff someone has to
read forever, and several of them together make the next bisect impossible.

Reverted-flat changes are worth roughly as much as landed ones, because they say where the cost
*isn't*. Write them down. Round 2's reverts: a struct-option `merge` (24 KB out of 15.3 MB), taking
package lookups out of a computation expression (wrong function -- there are several called
`withExtras`), a per-VM memo in front of a cache that was already warm, and disposing a leaked
`JsonDocument` (~350 bytes, not the 11 KB assumed).

## 6. Pick the workload deliberately, and re-pick it

**The mistake that defined round 2.** Every number in two campaigns came from one script. When six
workloads finally existed, that script was the *second-cheapest* of them, and three of the round's
biggest wins were invisible on it.

Run `perf-suite` (six shapes) and `perf-http` (the async path, under concurrency) after anything
structural. A change that helps one shape can cost another, and you cannot see that from inside one
workload.

Signs it's time to re-pick: returns are diminishing, the profile is flat, and the wins are getting
more specific to the thing you're measuring.

## 7. Verify past the test suite, by hand

The 10,119-test suite has **twice** passed while the interpreter returned a quietly wrong answer.
For anything touching execution, run `scripts/testing/perf-checks` and read the output --
self-recursion, enums, records, lambdas, matches, map/filter/fold, a polymorphic JSON round trip, a
deliberate type error, an error raised inside nested lambdas, and the four record construction
failures.

Error paths deserve special attention: they only run once something has already gone wrong, so a
change can garble every error message with every test green.

Two traps in writing such checks:

- **Structurally identical types are the same type** (names are content hashes), so a test passing a
  `B` where an `A` is expected is testing nothing. Make the shapes genuinely different.
- For behaviour-preservation work, **record the output before the change** and diff after. That's
  how the string fast path was validated: eighteen cases, byte-identical.

And run the suite with tracing **on** as well as off when touching anything the tracer reads -- it
reads a builtin's arguments *after* the body has returned, which is a lifetime nobody thinks about.

## 8. Refactoring a hot path without ending up with two of it

Hot paths grow a fast branch and a slow branch. Two rules learned the hard way:

- **Don't share a local between them.** F# can't lambda-lift a local a closure captures, so a value
  bound "just to save a line" becomes an allocation on every call to serve a branch that after the
  first call is never taken. Spell the call out in both arms.
- **Make the moved-from arm raise, not silently do nothing.** When `Apply` moved out of the
  computation expression, leaving its old arm as a no-op would have produced a silent wrong answer;
  raising caught it immediately.

The corollary bit me anyway: a rewrite meant to remove allocation *added* it, because the new helper
closed over an array instead of taking it as a parameter. 57% of the profile, created by the fix.
**When you extract a helper on a hot path, make it top-level and pass everything explicitly.**

## 9. Know what your binary actually contains

A Release build takes ten minutes, so it is tempting to measure against the one you built earlier.
Don't. I recorded a Release snapshot into the tracked benchmark history from a binary that predated
**five** subsequent commits, including the single biggest win of the round, and the numbers were
wrong in the direction that flatters nothing.

`build-release-cli-exes.sh` names the binary after `git rev-parse HEAD` *at build time*, which is
worse than no label: it looks authoritative and is wrong if the tree had uncommitted changes or has
moved on since. Before quoting a Release number, check the binary is newer than every commit you are
claiming credit for.

## 10. Working with the tooling here

- Run `perf-suite`, `perf-gate` and the rest in the **foreground**, with `< /dev/null`. Backgrounded
  shell commands are throttled here by ~50x, which reads as a regression.
- Pass `< /dev/null` to everything going through `scripts/run-in-docker`, or it hangs after
  finishing and every later command in the clone crawls. Never `pkill -f run-in-docker` -- it
  matches other clones and your own command line. Filter on `/proc/<pid>/cwd`.
- `scripts/dev/build` often doesn't exit after succeeding: redirect output and check
  `rundir/logs/build.log`.
- `alloc-profile` needs `--rundown false`. Without it the JIT rundown is ~80% of the trace and the
  profile looks flat when it isn't. This cost the campaign days.
- Environment variables do **not** survive the container re-entry these scripts do. Make it a flag.
- `ls a b` exits **non-zero when either operand is missing**, even though it prints the one that
  matched. Under `set -euo pipefail` that kills the script mid-assignment, with no message. Append
  `|| true` to any `EXE=$(ls a b | head -1)`.
- There are **two** places a published binary can be. CI's `build-backend` job runs `dotnet publish`
  and leaves it in `backend/Build/out/Cli/Release/.../publish/`; `build-cli` runs
  `build-release-cli-exes.sh`, which *moves* it to `clis/` and leaves the publish directory empty.
  Look in both. A tool that checks only one passes locally and fails in CI, or vice versa.
- `backend/Build/out` is a container volume and is not writable from the host. Staging anything
  there for a test has to happen inside the container.
- Debug builds in ~2 minutes, Release in ~10. Develop in Debug, decide in Release, and know that the
  two disagree by up to 3x on CE-heavy paths.

## 11. Leave the next person a gate, not a story

`scripts/testing/perf-gate` asserts allocation against a checked-in budget, separately for Debug and
published, and CI runs it. **Lower the budget in the same commit that earns it** -- a budget nobody
tightens stops being a gate and becomes a ceiling to drift up to.

**Keep CI's share of this small.** The gate is deliberately one script and a couple of seconds:
enough to catch a careless regression, not enough to slow every build. Do not put `perf-suite` or
`perf-http` in CI -- the suite runs twelve processes and `perf-http` starts a server and drives it
under load. Those are for a human, or a nightly, deciding something. A long perf job in the main
pipeline gets ignored and then disabled.

Same principle for documents: one roadmap, one history, one playbook, updated in place. Round 2
started with 58 markdown files of working notes across two repos, which is how you end up with five
sheets of paper and no idea which is current.
