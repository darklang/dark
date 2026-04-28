# Benchmarks

Allocation/timing micro-benchmarks for the Blob/Stream code paths.

```sh
./scripts/run-local-exec bench         # run scenarios, append snapshot
./scripts/run-local-exec bench-render  # rebuild results.md
```

`bench` appends one snapshot to the tracked `results/history.jsonl`
(and writes `results/latest.json` locally for tooling — gitignored).
`bench-render` reads the JSONL, renders the latest run as the
headline of `results.md`, and lists older runs in a compact table.

Scenarios live in `backend/src/LocalExec/Benchmarks.fs`. Each row
records input bytes, alloc bytes, elapsed ms, and Dval-node count.

## Local hacking

`results/local-*.json` is gitignored — drop a snapshot there to keep
it out of the tracked record.

## Why not in the test suite

These don't assert correctness, take seconds to run, and want
repeatable conditions (idle CPU, fresh GC) the test runner can't
guarantee.
