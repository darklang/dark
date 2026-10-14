#!/usr/bin/env python3
"""Compare a `perf/suite` run against the per-shape baselines in budget.json.

    python3 scripts/perf/_check-shapes.py <results.json> <budget.json> [--update]

Instructions per iteration are exactly reproducible, so they get a 2% band and are the real
assertion. Allocation is not: measured over three runs of one binary it moved 0.3% on `dicts` and
122% on `recursion`, because the figure is a small difference between two much larger ones. So it
gets a wide band plus an absolute slack, and shapes too small to difference are listed in
`shapeAllocNotAsserted` and checked on instructions alone.

Only increases fail. Exits 1 on a regression, 2 if it could not do the job. A shape with no
baseline is reported and passes, because a missing number must not read as a green check.
"""
import json
import sys

args = [a for a in sys.argv[1:] if not a.startswith("--")]
update = "--update" in sys.argv
if len(args) != 2:
    sys.exit("usage: _check-shapes.py <results.json> <budget.json> [--update]")

results = json.load(open(args[0]))
budget_path = args[1]
budget = json.load(open(budget_path))

shapes = budget.setdefault("shapes", {})
alloc_tol = budget.get("shapeAllocTolerance", 0.25)
alloc_slack = budget.get("shapeAllocSlackBytes", 512)
instr_tol = budget.get("shapeInstructionTolerance", 0.02)
no_alloc = set(budget.get("shapeAllocNotAsserted", []))

if update:
    for row in results:
        shapes[row["workload"]] = {
            "bytesPerIter": row["bytesPerIter"],
            "instructionsPerIter": row.get("instructionsPerIter"),
        }
    budget.setdefault("shapeAllocTolerance", alloc_tol)
    budget.setdefault("shapeAllocSlackBytes", alloc_slack)
    budget.setdefault("shapeInstructionTolerance", instr_tol)
    budget.setdefault("shapeAllocNotAsserted", sorted(no_alloc))
    with open(budget_path, "w") as f:
        json.dump(budget, f, indent=2)
        f.write("\n")
    print(f"wrote {len(results)} shape baselines to {budget_path}")
    sys.exit(0)

print()
print(f"{'shape':<12} {'alloc/iter':>12} {'vs base':>9} {'instr/iter':>12} {'vs base':>9}")
print(f"{'-' * 12:<12} {'-' * 12:>12} {'-' * 9:>9} {'-' * 12:>12} {'-' * 9:>9}")

failures = []
unpinned = []
for row in results:
    name = row["workload"]
    base = shapes.get(name)
    alloc = row["bytesPerIter"]
    instr = row.get("instructionsPerIter")

    def delta(now, then):
        if then in (None, 0) or now is None:
            return None
        return now / then - 1

    da = delta(alloc, (base or {}).get("bytesPerIter"))
    di = delta(instr, (base or {}).get("instructionsPerIter"))

    # A shape fails on allocation only if it moved both relatively and absolutely; see the note
    # at the top. Instructions have no slack, having no noise.
    alloc_base = (base or {}).get("bytesPerIter")
    alloc_moved = (
        name not in no_alloc
        and da is not None
        and da > alloc_tol
        and (alloc - alloc_base) > alloc_slack
    )

    def fmt(d, tol, flagged=None):
        if d is None:
            return "-"
        hit = (abs(d) > tol) if flagged is None else flagged
        return f"{d * 100:+.1f}%{' !' if hit else ''}"

    print(
        f"{name:<12} {alloc / 1024:>9.2f} KB {fmt(da, alloc_tol, alloc_moved):>9} "
        f"{(str(instr) if instr is not None else '?'):>12} {fmt(di, instr_tol):>9}"
        + ("   alloc not asserted" if name in no_alloc else "")
    )

    if base is None:
        unpinned.append(name)
        continue
    if alloc_moved:
        failures.append(
            f"{name}: allocation {da * 100:+.1f}% and {alloc - alloc_base:+.0f} B/iter, "
            f"past both the {alloc_tol * 100:.0f}% band and the {alloc_slack} B slack"
        )
    if di is not None and di > instr_tol:
        failures.append(
            f"{name}: instructions {di * 100:+.1f}% against a {instr_tol * 100:.0f}% band"
        )

print()
if unpinned:
    print(f"no baseline yet for: {', '.join(unpinned)} (pin with `scripts/perf/suite --release --update`)")

if failures:
    for f in failures:
        print(f"FAIL {f}")
    print("If the change earns these numbers, pin them in the same commit and say why.")
    sys.exit(1)

print(f"{len(results) - len(unpinned)} shape(s) within band.")
