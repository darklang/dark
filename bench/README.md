From a terminal run bench/run.py

# bench

Run `claude -p` against tasks in multiple languages and record
cost/turns/wall/pass-fail metrics.

## Invocations

| invocation | runs |
|---|---|
| `bench/run.py` | every task in `bench/tasks/*.md` × python+dark × 1 trial |
| `bench/run.py --task url-shortener-cli` | one task × python+dark × 1 trial |
| `bench/run.py --langs python --trials 3` | every task × python × 3 trials |
| `bench/run.py --task X --langs dark --trials 5` | one task × dark × 5 trials |

## Adding a task

Drop a markdown file at `bench/tasks/<name>.md` describing what to build.
The runner picks it up automatically on the next run.

The model gets your task md + a short language note (from `LANG_RULES` in
`run.py`) + "print PASS on the last line if it works." Pass/fail is graded
by reading the model's final reply.

## Output

- `bench/runs/<id>/` — one dir per trial: `prompt.md`, `claude_result.json`,
  `metrics.json`, and a `workspace/` with the seed `./run` shim plus whatever
  the model wrote. Gitignored.
- `bench/results.csv` — one row per trial with the curated metrics. Gitignored.
- `bench/report.md` — overwritten each run when any dark trial surfaces
  friction notes. The summary table itself is printed to stdout, not
  written here. Gitignored.
