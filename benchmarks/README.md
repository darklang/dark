# Benchmarks

Allocation/timing micro-benchmarks for the Blob/Stream code paths.
Three commands:

    ./scripts/run-local-exec bench
    ./scripts/run-local-exec bench-promote [--name <label>]
    ./scripts/run-local-exec bench-render

Scenarios live in `backend/src/LocalExec/Benchmarks.fs`. Each row
records input bytes, alloc bytes, elapsed ms, and Dval-node count.

## Workflow

`bench` runs every scenario and writes a fresh snapshot to
`results/latest.json`. Always overwrites. Also appends the snapshot
to `results/history.jsonl` (gitignored — local audit trail only).

`bench-promote` copies `latest.json` to `results/<label>.json`. The
copy becomes a tracked historic snapshot. Defaults the label to
today's date (`YYYY-MM-DD`) if `--name` is omitted. Use a label when
the snapshot represents a milestone (`pre-blob`, `post-l7`, etc.); use
the date when you just want a periodic record.

`bench-render` reads `latest.json` plus every committed snapshot in
`results/` and rewrites `results.md`. That file is the GitHub-UI
view — committing it alongside a new snapshot means reviewers see
table diffs in the PR.

## Local hacking

Snapshots named `local-*.json` are gitignored. Drop a file there if
you want to keep a measurement around without committing it:

    cp benchmarks/results/latest.json benchmarks/results/local-foo.json

Promote it later with `bench-promote --name foo` if you decide it
should be tracked.

## Why not in the test suite

These don't assert correctness, take seconds to run, and want
repeatable conditions (idle CPU, fresh GC) that test-suite runs
don't guarantee.

## Viewer

`viewer.html` renders `results/latest.json` interactively in a
browser — useful while iterating locally. `results.md` is the
canonical GitHub-UI view.
