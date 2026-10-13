#!/usr/bin/env python3
"""Interleaved cross-language timing for the programs in this directory.

Run from /home/dark/app inside the container, with DARK_EXE pointing at a published CLI:

    DARK_EXE=clis/darklang-alpha-<sha>-linux-x64 \
      python3 scripts/perf/workloads/crosslang/driver.py --rounds 7 --out rundir/crosslang.json

Why it is shaped this way:

- Round robin, not language by language. The box's load moves over minutes, so measuring all of
  one language and then all of the next bakes that drift into the comparison.
- The MINIMUM of the rounds is the number to read. A run can be slowed by the machine and never
  sped up by it, so the minimum is the least-noisy estimator available; the max is reported beside
  it because a wide spread is itself a finding.
- Every program prints `result=<v>` and the driver asserts all languages agree. A timing harness
  that does not check what it ran will happily time a program that crashed in 40 ms.
- Allocation is a separate pass, because measuring it perturbs the time: tracemalloc is slow and
  node needs a different heap configuration.
"""
import argparse
import json
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
DARK_EXE = os.environ.get("DARK_EXE")

SCENARIOS = [
    "hello",
    "lists",
    "strings",
    "dict",
    "records",
    "recursion-deep",
    "recursion-fib",
    "arith",
    "sort",
    "jsonrt",
    "fileread",
]

LANGS = ["dark", "python", "node", "bash"]
EXT = {"dark": "dark", "python": "py", "node": "js", "bash": "sh"}

# Where a language has no honest equivalent. A missing file is also skipped, but these say why.
EXCLUDED = {
    ("recursion-deep", "bash"): "bash segfaults at depth 5000 (it manages 4000)",
    ("records", "bash"): "bash has no record or struct type",
    ("jsonrt", "bash"): "bash has no JSON; jq would be a different program",
}


def program(scenario, lang, variant="workload"):
    """One directory per scenario, one file per language in it.

    Flat `<scenario>.<ext>` would be 45 files in one directory, and the alternative of keeping
    every language's source inside the driver and writing temp files was rejected: `_bench.py`
    did that and ended up with two copies of one workload that nothing kept in step.
    """
    return os.path.join(HERE, scenario, f"{variant}.{EXT[lang]}")


def command(scenario, lang, alloc=False):
    p = program(scenario, lang)
    if lang == "dark":
        return [DARK_EXE, "--no-trace", "run", p]
    if lang == "python":
        return ["python3", p]
    if lang == "node":
        if alloc:
            return ["node", "--expose-gc", "--max-semi-space-size=512", p]
        return ["node", p]
    return ["bash", p]


def applicable(scenario, lang):
    if (scenario, lang) in EXCLUDED:
        return False
    return os.path.exists(program(scenario, lang))


def run_once(scenario, lang, alloc=False):
    env = dict(os.environ)
    # The programs live one directory down now, and `import harness` resolves against sys.path.
    env["PYTHONPATH"] = HERE + (os.pathsep + env["PYTHONPATH"] if env.get("PYTHONPATH") else "")
    if alloc:
        env["CROSSLANG_ALLOC"] = "1"
        env["DARK_TELEMETRY"] = "1"
        env["DARK_CONFIG_TRACE_DETAIL"] = "off"
    t0 = time.perf_counter()
    p = subprocess.run(command(scenario, lang, alloc), capture_output=True, text=True, env=env)
    ms = (time.perf_counter() - t0) * 1000
    result = None
    for line in p.stdout.splitlines():
        if line.startswith("result="):
            result = line[len("result="):].strip()
    alloc_bytes = None
    alloc_peak = None
    for line in p.stderr.splitlines():
        if line.startswith("alloc_bytes="):
            alloc_bytes = int(line.split("=")[1])
        if line.startswith("alloc_peak_bytes="):
            alloc_peak = int(line.split("=")[1])
    return {
        "ms": ms,
        "rc": p.returncode,
        "result": result,
        "alloc_bytes": alloc_bytes,
        "alloc_peak_bytes": alloc_peak,
        "stderr": p.stderr[-400:] if p.returncode != 0 else "",
    }


FIXTURE = "rundir/crosslang-lines.txt"
FIXTURE_LINES = 100000


def setup():
    """Write the file-reading fixture, and let the Dark binary read it.

    Dark's instance policy denies filesystem reads by default, so without the grant the fileread
    row exits 1 in 40 ms, which is indistinguishable from a very fast pass if nobody checks the
    result. The grant is scoped to the one path and is idempotent.
    """
    if not os.path.exists(FIXTURE) or sum(1 for _ in open(FIXTURE)) != FIXTURE_LINES:
        with open(FIXTURE, "w") as f:
            for i in range(1, FIXTURE_LINES + 1):
                level = "ERROR" if i % 7 == 0 else "INFO"
                f.write(f"2026-10-13T00:00:00 line {i} LEVEL={level} payload=abcdefgh\n")
    path = os.path.join(os.environ["DARK_CONFIG_RUNDIR"], "crosslang-lines.txt")
    subprocess.run([DARK_EXE, "permissions", "allow", "file", "read", path],
                   capture_output=True, text=True)


def loadavg():
    with open("/proc/loadavg") as f:
        return [float(x) for x in f.read().split()[:3]]


def dark_alloc(scenario):
    """Bytes the whole Dark process allocated, from the gc.stats telemetry event."""
    path = os.path.join(os.environ["DARK_CONFIG_RUNDIR"], "logs", "telemetry.jsonl")
    if os.path.exists(path):
        os.truncate(path, 0)
    r = run_once(scenario, "dark", alloc=True)
    found = []
    try:
        with open(path) as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    rec = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if rec.get("event") == "gc.stats":
                    found.append(int(rec["ctx"]["totalAllocatedBytes"]))
    except FileNotFoundError:
        return None, r
    return (found[-1] if found else None), r


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rounds", type=int, default=7)
    ap.add_argument("--only", nargs="*", default=None)
    ap.add_argument("--out", default=None)
    ap.add_argument("--alloc", action="store_true", help="also run the allocation pass")
    args = ap.parse_args()

    if not DARK_EXE:
        sys.exit("driver: set DARK_EXE to the published CLI")

    scenarios = args.only or SCENARIOS
    setup()
    load_before = loadavg()

    # One warm-up pass, not recorded: the first Dark invocation against a store may seed it, and
    # the page cache for every binary is cold.
    for s in scenarios:
        for lang in LANGS:
            if applicable(s, lang):
                run_once(s, lang)

    samples = {(s, l): [] for s in scenarios for l in LANGS if applicable(s, l)}
    results = {}
    failures = []
    loads = [loadavg()[0]]

    for rnd in range(args.rounds):
        for s in scenarios:
            for lang in LANGS:
                if not applicable(s, lang):
                    continue
                r = run_once(s, lang)
                if r["rc"] != 0 or r["result"] is None:
                    failures.append((s, lang, r["rc"], r["stderr"]))
                samples[(s, lang)].append(r["ms"])
                results.setdefault((s, lang), r["result"])
        loads.append(loadavg()[0])
        print(f"round {rnd + 1}/{args.rounds} done, load1={loads[-1]}", file=sys.stderr)

    load_after = loadavg()

    # Every language must agree on the answer, or the programs are not the same program.
    mismatches = []
    for s in scenarios:
        vals = {l: results[(s, l)] for l in LANGS if (s, l) in results}
        if len(set(vals.values())) > 1:
            mismatches.append((s, vals))

    out = {
        "rounds": args.rounds,
        "loadavg_before": load_before,
        "loadavg_after": load_after,
        "loadavg_per_round": loads,
        "excluded": {f"{k[0]}/{k[1]}": v for k, v in EXCLUDED.items()},
        "mismatches": [{"scenario": s, "results": v} for s, v in mismatches],
        "failures": [{"scenario": s, "lang": l, "rc": rc, "stderr": e} for s, l, rc, e in failures],
        "timing": {},
        "alloc": {},
    }
    for (s, lang), ms in samples.items():
        out["timing"].setdefault(s, {})[lang] = {
            "min_ms": round(min(ms), 2),
            "max_ms": round(max(ms), 2),
            "n": len(ms),
            "result": results[(s, lang)],
        }

    if args.alloc:
        for s in scenarios:
            for lang in LANGS:
                if not applicable(s, lang) or lang == "bash":
                    continue
                if lang == "dark":
                    total, r = dark_alloc(s)
                    out["alloc"].setdefault(s, {})["dark"] = {
                        "process_bytes": total, "result": r["result"]}
                else:
                    r = run_once(s, lang, alloc=True)
                    out["alloc"].setdefault(s, {})[lang] = {
                        "work_bytes": r["alloc_bytes"],
                        "peak_bytes": r["alloc_peak_bytes"],
                        "result": r["result"]}

    text = json.dumps(out, indent=2)
    if args.out:
        with open(args.out, "w") as f:
            f.write(text + "\n")
        print(f"wrote {args.out}", file=sys.stderr)
    else:
        print(text)

    # Table, so a run is readable without the JSON.
    print()
    print(f"{'scenario':<16}" + "".join(f"{l:>22}" for l in LANGS))
    for s in scenarios:
        row = f"{s:<16}"
        for lang in LANGS:
            t = out["timing"].get(s, {}).get(lang)
            cell = "n/a" if t is None else "%.1f / %.1f" % (t["min_ms"], t["max_ms"])
            row += f"{cell:>22}"
        print(row)
    print("\nmin / max ms over", args.rounds, "rounds; load1",
          f"{min(loads):.2f}-{max(loads):.2f}")

    # Allocation beside time: time is what a person waits for and drifts with the box, allocation
    # barely moves between runs and is where a change to the interpreter shows up first.
    #
    # The cells are NOT comparable across languages and the marks say so. Dark reports what the
    # whole process allocated, because that is what its counters can see; python's tracemalloc and
    # node's heapUsed delta bracket the measured region only. So read a column down the commits,
    # not a row across the languages. Bash reports nothing.
    if out["alloc"]:
        print()
        print(f"{'scenario':<16}" + "".join(f"{l:>22}" for l in LANGS))
        for s_ in scenarios:
            row = f"{s_:<16}"
            for lang in LANGS:
                a = out["alloc"].get(s_, {}).get(lang) or {}
                work, whole = a.get("work_bytes"), a.get("process_bytes")
                if work is not None:
                    cell = f"{work / 1e6:.2f} MB"
                elif whole is not None:
                    cell = f"{whole / 1e6:.2f} MB *"
                else:
                    cell = "n/a"
                row += f"{cell:>22}"
            print(row)
        print("\nallocated, one run each. * whole process, not just the measured region, so it")
        print("carries startup and package loading. The unmarked cells are the region only.")
    if mismatches:
        print("MISMATCHED RESULTS:", mismatches)
    if failures:
        print("FAILURES:", failures[:5])


if __name__ == "__main__":
    main()
