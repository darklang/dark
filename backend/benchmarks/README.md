# Benchmarks

Allocation/timing micro-benchmarks for the Blob/Stream code paths. Run via:

    ./scripts/run-local-exec bench

Writes a fresh snapshot to `results/latest.json` (committed, so the diff
shows the change) and appends the same snapshot to `results/history.jsonl`
(local-only).

Scenarios live in `backend/src/LocalExec/Benchmarks.fs`. Each row records
input bytes, alloc bytes, elapsed ms, and Dval-node count for one run.

Open `viewer.html` in a browser to see the latest results as a table —
including the `overhead = allocBytes / inputBytes` ratio that's the
shape we care about.

These are deliberately not part of the test suite: they don't assert
correctness, take seconds to run, and want repeatable conditions
(idle CPU, fresh GC) that test-suite runs don't guarantee.
