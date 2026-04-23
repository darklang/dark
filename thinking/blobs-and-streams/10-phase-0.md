# Phase 0 — Baseline Measurement

Lock in current behaviour before any refactor so we can prove the
work did what it claimed. Results live in
`thinking/blobs-and-streams/baseline.md` (created by chunk 0.6).

Phase 0 should take one session of work. Cheap and fully reversible.

## What to measure

We want numbers, not just tests. The harness captures these for each
scenario:

| Metric                       | How                                                       | Why                                                 |
| ---------------------------- | --------------------------------------------------------- | --------------------------------------------------- |
| Managed bytes allocated      | `GC.GetTotalAllocatedBytes(precise=false)` delta          | Monotonic counter; before/after diff                |
| Peak working set (RSS)       | `Process.GetCurrentProcess().WorkingSet64` polled during  | Physical RAM; noisy but catches unmanaged growth    |
| Wall time                    | `Stopwatch.StartNew()` / `.Elapsed`                       | End-to-end cost                                     |
| Dval node count              | Walker that counts Dval allocations in a result tree      | Exposes boxed-list overhead directly                |
| Time to first byte / chunk   | Stopwatch around first chunk emission                     | Streaming behaviour                                 |
| SQLite file size             | `FileInfo(rundir/packages.sqlite).Length`                 | Establishes Phase 1 `package_blobs` delta           |

The walker is a new F# helper; ~30 LOC in
`backend/tests/Tests/Measurement.Helpers.fs` (new file). Counts every
Dval node reachable from a root, including list cells, record
fields, etc.

## Scenarios

1. **`fileRead` memory footprint by size.**
   - Inputs: temp files of 1 KB, 100 KB, 1 MB, 10 MB, 38 MB.
   - Capture: allocation delta, peak RSS, wall time, Dval node count
     of the returned `List<UInt8>`.
   - Expect: linear-ish allocation and node count until ~10 MB, then
     falls off a cliff at 38 MB (OOM).
2. **HttpClient body by size.**
   - Fire a test HTTP server on a loopback port returning 100 KB /
     1 MB / 10 MB payloads. (Standalone in-process Kestrel; don't
     depend on internet.)
   - Capture the same metrics on `HttpClient.request`'s body.
3. **StreamingHttpClient chunk behaviour.**
   - Serve a 10 MB response from the same loopback server, one chunk
     per 100 ms. Time to first chunk, time to last chunk,
     per-chunk allocation.
4. **`Builtin.bytesHexEncode` cost.**
   - Input: 1 MB of random bytes as `List<UInt8>`.
   - Capture: allocation, wall time.
5. **Baseline SQLite file size.**
   - Just record the starting `FileInfo(..., packages.sqlite).Length`
     so Phase 1 can diff.

## Where to record results

- **Raw results:** `thinking/blobs-and-streams/baseline.md` —
  Markdown tables and prose commentary. Commit this file as part of
  chunk 0.6. It stays authoritative for the rest of the work.
- **Reproducibility:** the measurement harness itself lives in
  `backend/tests/Tests/Measurement.fs` (new file). Each scenario is
  a `testTask` that writes its row to a per-test file in
  `rundir/measurements/phase-0/*.txt`. Runs with
  `./scripts/run-backend-tests --filter-test-list Measurement`.
- **Not in package manager.** Originally considered; the overhead of
  parsing numbers into Dark values for an ad-hoc benchmarking
  workflow outweighs the benefit. Revisit if benchmarking becomes
  recurring work.

## Manual testing for Phase 0

- Run the harness once, eyeball `baseline.md` for obviously-wrong
  values (e.g. "0 bytes allocated" means something's inlined).
- Compare the fileRead 38 MB number against the `~49 GB` anecdote
  from bug-fileread-oom.md; if it's very different, note it —
  something's changed since that writeup.

## Chunks

### 0.1 — add baseline-measurement harness

Create:
- `backend/tests/Tests/Measurement.fs` — skeleton with one dummy
  test that exercises `GC.GetTotalAllocatedBytes` and
  `Process.WorkingSet64` to make sure the plumbing works.
- `backend/tests/Tests/Measurement.Helpers.fs` — Dval node-count
  walker.
- Register both in `backend/tests/Tests/Tests.fsproj`.

**Done when:** `./scripts/run-backend-tests --filter-test-list Measurement` runs the dummy test and produces one output file in `rundir/measurements/phase-0/`.

**Tests:**
- F#: the dummy measurement test in `Measurement.fs`.
- Dark: none.

**Commit:** `phase-0: add measurement harness skeleton`

### 0.2 — record fileRead memory/allocation profile

Implement scenario 1. Write five rows (1KB, 100KB, 1MB, 10MB, 38MB)
to `rundir/measurements/phase-0/fileRead.txt`.

**Done when:** all five rows present. 38 MB row may be `OOM: >N GB`
string if it OOMs — that's fine, capture the observation.

**Tests:** the scenario itself. Pass criterion: all rows produced;
no assertions yet (this is measurement, not regression).

**Commit:** `phase-0: measure fileRead by file size`

### 0.3 — record http body allocation profile

Implement scenario 2. Spin up loopback Kestrel for the test session
only; test writes 100KB, 1MB, 10MB bodies.

**Done when:** `rundir/measurements/phase-0/httpBody.txt` has three
rows.

**Tests:** scenario 2.

**Commit:** `phase-0: measure httpclient body allocation by size`

### 0.4 — record streaming-http chunk behaviour

Scenario 3. Chunked transfer, one 100KB chunk per 100ms, 100 chunks.

**Done when:** `rundir/measurements/phase-0/streaming.txt` records
time-to-first-chunk, time-to-last-chunk, and per-chunk allocation
average.

**Tests:** scenario 3.

**Commit:** `phase-0: measure streaming-http chunk behaviour`

### 0.5 — record bytesHexEncode cost on 1MB

Scenario 4.

**Done when:** `rundir/measurements/phase-0/hex.txt` has the one
row.

**Commit:** `phase-0: measure bytesHexEncode on 1mb input`

### 0.6 — snapshot sqlite size and write results into baseline.md

- Record `FileInfo(rundir/packages.sqlite).Length` before any blob
  work — include in `baseline.md`.
- Convert the per-scenario `.txt` files into a single
  `thinking/blobs-and-streams/baseline.md`:
  - one section per scenario
  - markdown table per scenario with the metrics
  - short prose paragraph noting what's surprising
  - explicit "Post-Phase-1 target" lines where we can guess
    (e.g. "fileRead 38 MB: expect <100 MB allocated, <1 s wall")

**Done when:** `baseline.md` is committed and legible as a standalone
report.

**Tests:** none new; this is writing.

**Commit:** `phase-0: write baseline.md from measurement runs`
