# How to do performance work on Dark

Method, not results. The numbers live in `docs/perf/history.md`, what to do next in
`docs/perf/roadmap.md`.

Picking this up cold: read this, then the roadmap, then run `scripts/perf/suite` to see where things
actually are before believing anything.

The one-line version: **nearly all wasted effort came from trusting a measurement nobody had
checked.** Everything below is a way of not doing that.

---

## 1. Measure allocation, not time

Allocation for a fixed workload repeats **to the byte** and doesn't care how loaded the machine is.
Time on this box drifts about 12 ms, enough to hide or invent most individual wins; eleven runs
cannot reliably resolve a 10 ms difference.

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

## 9. Know what your binary actually contains

A Release build takes ten minutes, so measuring against the one you built earlier is tempting. Don't.
`build-release-cli-exes.sh` names the binary after `git rev-parse HEAD` *at build time*, which is
worse than no label: it looks authoritative and is wrong if the tree had uncommitted changes or has
moved on. Before quoting a Release number, check the binary is newer than every commit you are
crediting.

## 10. Working with the tooling here

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
- There are **two** places a published binary can be: CI's `build-backend` job runs `dotnet publish`
  and leaves it in the publish directory, while `build-cli` runs `build-release-cli-exes.sh`, which
  *moves* it to `clis/` and empties that one. `scripts/perf/_common` handles both; use it.
- `backend/Build/out` is a container volume and is not writable from the host. Staging anything there
  has to happen inside the container.
- Debug builds in ~2 minutes, Release in ~10. Develop in Debug, decide in Release, and know the two
  disagree by up to 3x on paths still heavy in computation expressions.

## 11. Leave the next person a gate, not a story

`scripts/perf/gate` asserts allocation against a checked-in budget, separately for Debug and
published, and CI runs it. **Lower the budget in the same commit that earns it** -- a budget nobody
tightens stops being a gate and becomes a ceiling to drift up to.

**Keep CI's share small.** The gate is deliberately one script and a couple of seconds. Don't put
`scripts/perf/suite` or `scripts/perf/http` in CI: the suite runs twelve processes, and `http` starts
a server and drives it under load. Those are for a human, or a nightly, deciding something. A long
perf job in the main pipeline gets ignored and then disabled.

Same for documents: one roadmap, one history, one playbook, updated in place. The alternative is
dozens of working notes and no way to tell which is current.
