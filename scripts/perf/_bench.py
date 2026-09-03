#!/usr/bin/env python3
"""Repeatable CLI timing for the perf campaign.

A number you can't reproduce is not a measurement. Four things go wrong without this, and all four have
bitten this project:

  - The store drifts. Traces and ops accumulate run over run, so the same command gets slower for reasons
    that have nothing to do with the code. Every run here starts from a byte-identical fixture.
  - `config/dev` sets DARK_CONFIG_TRACE_DETAIL=on, so a dev run records a full execution trace and a user's
    never does. Inherited silently, it lands in every number. Pinned explicitly here, and reported.
  - The box is shared. Four dark containers have been live at once on this machine, and the historical
    cli.execute spread is 5x on runs doing identical work. A datapoint taken while a neighbour compiles is
    noise wearing a number's clothes, so we refuse to record one.
  - Measure-A, change code, measure-B drifts. Comparisons here interleave two binaries A,B,A,B and report
    the median of the PAIRED difference, which is immune to slow drift across the batch.

Usage:
  perf-bench scenarios
  perf-bench fixture save
  perf-bench bin save <name>
  perf-bench run <scenario> [-n N] [--trace on|off] [--allow-noise]
  perf-bench ab <binA> <binB> <scenario> [-n N] [--trace on|off]
"""

import argparse
import hashlib
import json
import os
import shutil
import statistics
import subprocess
import sys
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
RUNDIR = os.path.join(ROOT, "rundir")
PERFDIR = os.path.join(RUNDIR, "perf")
FIXTURE = os.path.join(PERFDIR, "fixture.db")


def fixture_path(name):
    """`none` means don't restore anything: use whatever store the binary already has.

    A published binary carries its own embedded seed and extracts it beside the exe, so two published
    binaries are each already matched to their own store. Forcing a shared fixture onto them would put one
    of them next to package code that calls builtins it doesn't have."""
    if name == "none":
        return None
    """A change that spans both F# builtins and Dark packages can't be A/B'd by swapping binaries alone:
    the store holds the package implementations, and they call builtins that only exist in one of the two
    binaries. Old binary plus new store fails, and so does the reverse. Each side needs its own fixture,
    captured while that side was live."""
    return FIXTURE if name in (None, "default") else os.path.join(PERFDIR, f"fixture-{name}.db")
BINDIR = os.path.join(PERFDIR, "bin")
DEBUG_BIN_DIR = os.path.join(ROOT, "backend", "Build", "out", "Cli", "Debug", "net10.0")
# Default only; the real path depends on which rundir the binary uses. See `rundir_for`.
TELEMETRY = os.path.join(RUNDIR, "logs", "telemetry.jsonl")
# Raw measurements are machine-local: they name a binary, a store fixture and a load average that mean
# nothing on another box. `notes/` is gitignored in this repo anyway (the real notes live in the sibling
# notes repo, which isn't mounted in the container), so `rundir/` is both the honest home and the reachable
# one. Rendered summaries go to the notes repo by hand.
SERIES = os.path.join(PERFDIR, "timeseries.jsonl")

# Every scenario is one non-interactive CLI invocation. Interactive ones need a tmux driver and land once
# the per-keystroke instrument is cheap enough to trust (see plan 6.B); measuring them now would only
# measure the instrument.
SCENARIOS = {
    "version": ["version"],
    "status": ["status"],
    "help": ["help"],
    "tree": ["tree"],
    "eval-trivial": ["eval", "1L + 2L"],
    "eval-map5": ["eval", "Stdlib.List.map [1,2,3,4,5] (fun x -> x + 1)"],
    "eval-map1000": ["eval", "Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 1000) (fun x -> x + 1))"],
    # Heavy enough that the interpreter, not process startup, dominates. Startup is ~400 ms of fixed cost,
    # so a scenario doing 20 ms of list work can't show a list-work improvement at all.
    "eval-listheavy": ["eval",
        "Stdlib.List.length (Stdlib.List.filter (Stdlib.List.map (Stdlib.List.range 1 2000) (fun x -> x + 1)) (fun x -> x > 5))"],
    # Interpreter workloads, materialized by `materialize_workloads`.
    "interp-list": ["run", "rundir/perf-workloads/steady.dark"],
    "interp-arith": ["run", "rundir/perf-workloads/arith.dark"],
    "eval-flatten": ["eval",
        "Stdlib.List.length (Stdlib.List.flatten (Stdlib.List.map (Stdlib.List.range 1 400) (fun x -> Stdlib.List.range 1 20)))"],
}

# Interpreter workloads, kept as source here rather than as .dark files in the tree: anything under
# `packages/` would be loaded as package items, and a loose file elsewhere drifts from the harness that
# depends on it. Materialized into rundir (gitignored) on demand, so a scenario is reproducible from this
# file alone.
#
# These exist because measuring the interpreter by hand is how I burned a night: script runs that timed out
# at 90s, 240s and 500s all later ran in under a second, because the box was busy and I'd bypassed the noise
# gate by invoking run-cli directly. Behind a scenario, they inherit the gate.
WORKLOADS = {
    # List pipeline: package-call heavy, shallow recursion.
    "steady": """
let work (n: Int) : Int =
  Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 n) (fun x -> x + 1))

let repeat (times: Int) (n: Int) (acc: Int) : Int =
  if times <= 0 then acc else repeat (times - 1) n (acc + (work n))

let _warm = repeat 20 50 0
let _reset = Builtin.interpreterStatsReset ()
let t0 = Builtin.timeNowMs ()
let result = repeat 200 50 0
let t1 = Builtin.timeNowMs ()
Stdlib.printLine ("elapsed_ms=" ++ (Stdlib.Int.toString (t1 - t0)) ++ " stats=" ++ (Builtin.interpreterStatsGet ()))
""",
    # Arithmetic: builtin-call heavy, deep recursion. A deliberately different shape from `steady`, since
    # the two disagree substantially on per-Apply cost.
    "arith": """
let hot (n: Int) (acc: Int) : Int =
  if n <= 0 then
    acc
  else
    let a = acc + 1
    let b = a + 2
    let c = b + 3
    let d = c + 4
    let e = d + 5
    hot (n - 1) e

let _warm = hot 200 0
let _reset = Builtin.interpreterStatsReset ()
let t0 = Builtin.timeNowMs ()
let result = hot 4000 0
let t1 = Builtin.timeNowMs ()
Stdlib.printLine ("elapsed_ms=" ++ (Stdlib.Int.toString (t1 - t0)) ++ " stats=" ++ (Builtin.interpreterStatsGet ()))
""",
}

WORKLOADS["depth-shallow"] = """
// Same total work as depth-deep, but the stack never gets deeper than ~50.
// Three nested drivers of 16 keep level small; a single driver would itself recurse 4096 deep, which is
// what made my first attempt at this comparison measure nothing.
let leaf (acc: Int) : Int =
  acc + 1

let l1 (n: Int) (acc: Int) : Int =
  if n <= 0 then acc else l1 (n - 1) (leaf acc)

let l2 (n: Int) (acc: Int) : Int =
  if n <= 0 then acc else l2 (n - 1) (l1 16 acc)

let l3 (n: Int) (acc: Int) : Int =
  if n <= 0 then acc else l3 (n - 1) (l2 16 acc)

let _warm = l3 2 0
let t0 = Builtin.timeNowMs ()
let r = l3 16 0
let t1 = Builtin.timeNowMs ()
Stdlib.printLine ("elapsed_ms=" ++ (Stdlib.Int.toString (t1 - t0)) ++ " leafCalls=" ++ (Stdlib.Int.toString r))
"""

WORKLOADS["depth-deep"] = """
// Same 4096 leaf calls, but all in one chain, so 4096 frames are live at the deepest point.
let leaf (acc: Int) : Int =
  acc + 1

let chain (n: Int) (acc: Int) : Int =
  if n <= 0 then acc else chain (n - 1) (leaf acc)

let _warm = chain 32 0
let t0 = Builtin.timeNowMs ()
let r = chain 4096 0
let t1 = Builtin.timeNowMs ()
Stdlib.printLine ("elapsed_ms=" ++ (Stdlib.Int.toString (t1 - t0)) ++ " leafCalls=" ++ (Stdlib.Int.toString r))
"""

SCENARIOS["depth-shallow"] = ["run", "rundir/perf-workloads/depth-shallow.dark"]
SCENARIOS["depth-deep"] = ["run", "rundir/perf-workloads/depth-deep.dark"]

WORKLOAD_DIR = os.path.join(RUNDIR, "perf-workloads")


def materialize_workloads():
    os.makedirs(WORKLOAD_DIR, exist_ok=True)
    for name, src in WORKLOADS.items():
        with open(os.path.join(WORKLOAD_DIR, f"{name}.dark"), "w") as f:
            f.write(src.lstrip())


# A run slower than this is assumed to have hit something pathological (a lock, a neighbour's compile
# landing mid-run) and is reported rather than silently folded into the median.
OUTLIER_FACTOR = 3.0


# ---------------------------------------------------------------- environment


def load_average():
    with open("/proc/loadavg") as f:
        return float(f.read().split()[0])


def noise_report():
    """What's competing with us right now. Load average is the host's; container namespaces don't
    virtualise it, which is exactly what we want to know."""
    la = load_average()
    cpus = os.cpu_count() or 1
    # dotnet/msbuild running anywhere on the box means a neighbour clone is compiling.
    try:
        out = subprocess.run(
            ["pgrep", "-af", "dotnet|MSBuild|ilc"],
            capture_output=True, text=True, timeout=10,
        ).stdout
        compilers = [l for l in out.splitlines() if "perf_bench" not in l and "pgrep" not in l]
    except Exception:
        compilers = []
    return {"loadavg1": la, "cpus": cpus, "load_per_cpu": la / cpus, "compilers": len(compilers)}


def noise_gate(allow):
    n = noise_report()
    # A quarter of the box busy is enough to move a median. The threshold is deliberately strict: a
    # rejected batch costs minutes, a bad number costs a day chasing it.
    noisy = n["load_per_cpu"] > 0.25 or n["compilers"] > 0
    if noisy and not allow:
        print(
            f"REFUSING to measure: loadavg {n['loadavg1']:.1f} over {n['cpus']} cpus "
            f"({n['load_per_cpu']:.2f}/cpu), {n['compilers']} compiler processes running.\n"
            f"Wait for the box to settle, or pass --allow-noise to record anyway (the row is "
            f"flagged noisy and should not be compared against a quiet one).",
            file=sys.stderr,
        )
        sys.exit(2)
    return n, noisy


# ---------------------------------------------------------------- fixture


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def fixture_save(name=None):
    os.makedirs(PERFDIR, exist_ok=True)
    dest = fixture_path(name)
    src = os.path.join(RUNDIR, "data.db")
    if not os.path.exists(src):
        sys.exit("no rundir/data.db to snapshot")
    # Checkpoint first so the fixture is one self-contained file rather than a db plus a WAL whose
    # contents would be silently dropped by the copy.
    subprocess.run(
        ["sqlite3", src, "PRAGMA wal_checkpoint(TRUNCATE);"],
        check=False, capture_output=True,
    )
    shutil.copy2(src, dest)
    digest = sha256(dest)
    with open(dest + ".sha256", "w") as f:
        f.write(digest + "\n")
    print(f"fixture saved: {dest}")
    print(f"  {os.path.getsize(dest):,} bytes")
    print(f"  sha256 {digest}")


def rundir_for(binary):
    """Which store a given binary actually uses.

    An explicit DARK_CONFIG_RUNDIR wins for every binary, published or Debug, so when it is set that IS
    the answer. `<exe dir>/.darklang` is only the default a published binary computes for itself when the
    variable is unset, and a leftover directory next to an exe must not outrank the store we were told
    to use.

    Getting this wrong silently compares two binaries against two different stores, which is how a
    harness ends up lying more convincingly than no harness at all."""
    if os.environ.get("DARK_CONFIG_RUNDIR"):
        return os.environ["DARK_CONFIG_RUNDIR"]
    adjacent = os.path.join(os.path.dirname(os.path.abspath(binary)), ".darklang")
    return adjacent if os.path.isdir(adjacent) else RUNDIR


def fixture_restore(rundir, fixture=None):
    if fixture == "SKIP":
        return
    """Byte-identical store before every run. The `-wal` and `-shm` must go too: leaving them turns the
    restored db into a db plus someone else's journal, which is a different store and sometimes a corrupt
    one."""
    fixture = fixture or FIXTURE
    if not os.path.exists(fixture):
        sys.exit(f"no fixture at {fixture}; run `perf-bench fixture save [name]` first")
    for suffix in ("", "-wal", "-shm"):
        p = os.path.join(rundir, "data.db" + suffix)
        if os.path.exists(p):
            os.remove(p)
    shutil.copy2(fixture, os.path.join(rundir, "data.db"))


# ---------------------------------------------------------------- binaries


def bin_save(name):
    os.makedirs(BINDIR, exist_ok=True)
    dst = os.path.join(BINDIR, name)
    if not os.path.isdir(DEBUG_BIN_DIR):
        sys.exit(f"no build at {DEBUG_BIN_DIR}")
    if os.path.exists(dst):
        shutil.rmtree(dst)
    # The whole output directory, not just the exe: the deps and runtimeconfig travel with it, which is
    # what makes an old binary runnable next to a new one.
    shutil.copytree(DEBUG_BIN_DIR, dst)
    print(f"binary saved: {dst} (from {DEBUG_BIN_DIR})")


def resolve_bin(name):
    """A saved snapshot name, the live build, or a path to any other Cli."""
    if name in ("current", "live"):
        return os.path.join(DEBUG_BIN_DIR, "Cli")
    # A path lets you A/B against something published elsewhere (a Release build, an R2R publish)
    # without copying it into the snapshot dir first.
    if "/" in name:
        p = name if os.path.basename(name) == "Cli" else os.path.join(name, "Cli")
        if not os.path.exists(p):
            sys.exit(f"no Cli at {p}")
        return os.path.abspath(p)
    p = os.path.join(BINDIR, name, "Cli")
    if not os.path.exists(p):
        sys.exit(f"no saved binary {name!r} (looked in {p})")
    return p


# ---------------------------------------------------------------- running


def run_once(binary, argv, trace, telemetry, fixture="UNSET"):
    rundir = rundir_for(binary)
    fixture_restore(rundir, "SKIP" if fixture is None else (None if fixture == "UNSET" else fixture))
    tel_path = os.path.join(rundir, "logs", "telemetry.jsonl")
    if os.path.exists(tel_path):
        os.remove(tel_path)
    env = dict(os.environ)
    # Pinned, never inherited: config/dev turns tracing on for the container, so a run that doesn't say
    # otherwise is measuring the traced path without meaning to.
    env["DARK_CONFIG_TRACE_DETAIL"] = trace
    if telemetry:
        env["DARK_TELEMETRY"] = "1"
    else:
        env.pop("DARK_TELEMETRY", None)

    t0 = time.perf_counter()
    proc = subprocess.run(
        [binary] + argv, capture_output=True, text=True, env=env, cwd=ROOT
    )
    wall_ms = (time.perf_counter() - t0) * 1000.0
    return wall_ms, proc.returncode, read_telemetry(tel_path) if telemetry else {}


def read_telemetry(path):
    """Phase spans and counters from the run that just finished. Durations only; the counter events carry
    their value in ctx instead."""
    out = {}
    if not os.path.exists(path):
        return out
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                e = json.loads(line)
            except Exception:
                continue
            name = e.get("event")
            if "ms" in e:
                out[name] = e["ms"]
            ctx = e.get("ctx") or {}
            for k in ("count", "instructions", "packageCalls", "builtinCalls", "framePushes"):
                if k in ctx:
                    out[f"{name}.{k}" if k == "count" else k] = int(ctx[k])
    return out


def summarise(samples):
    s = sorted(samples)
    n = len(s)
    if n == 0:
        return {}
    q1 = s[n // 4]
    q3 = s[(3 * n) // 4]
    return {
        "n": n,
        "min": round(s[0], 2),
        "median": round(statistics.median(s), 2),
        "p90": round(s[min(n - 1, int(0.9 * n))], 2),
        "max": round(s[-1], 2),
        "iqr": round(q3 - q1, 2),
    }


def commit():
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True, cwd=ROOT,
        ).stdout.strip()
    except Exception:
        return "unknown"


def fixture_digest():
    p = FIXTURE + ".sha256"
    return open(p).read().strip()[:12] if os.path.exists(p) else "none"


class DevStore:
    """Put the developer's store back when the batch ends.

    Every measured run overwrites `rundir/data.db` with a fixture and leaves it there, so after a batch the
    working tree is sitting on whichever store the last run used. If that fixture predates a `packages/`
    change, the tree is now mismatched with the binary's regenerated PackageRefs, and the symptom is a
    `FnNotFound` on `RuntimeError.toString` -- i.e. every subsequent error fails to stringify and reports
    the wrong thing. That cost me hours before I noticed the harness was the cause."""

    def __init__(self):
        self.saved = None

    def __enter__(self):
        src = os.path.join(RUNDIR, "data.db")
        if os.path.exists(src):
            self.saved = os.path.join(PERFDIR, "devstore-backup.db")
            os.makedirs(PERFDIR, exist_ok=True)
            shutil.copy2(src, self.saved)
        return self

    def __exit__(self, *exc):
        if self.saved and os.path.exists(self.saved):
            for suffix in ("", "-wal", "-shm"):
                p = os.path.join(RUNDIR, "data.db" + suffix)
                if os.path.exists(p):
                    os.remove(p)
            shutil.move(self.saved, os.path.join(RUNDIR, "data.db"))
            print("\n(restored the dev store)")
        return False


def record(row):
    row.setdefault("fixture", fixture_digest())
    os.makedirs(os.path.dirname(SERIES), exist_ok=True)
    with open(SERIES, "a") as f:
        f.write(json.dumps(row, sort_keys=True) + "\n")


# ---------------------------------------------------------------- commands


def cmd_run(args):
    materialize_workloads()
    with DevStore():
        if args.scenario not in SCENARIOS:
            sys.exit(f"unknown scenario {args.scenario!r}; try `perf-bench scenarios`")
        argv = SCENARIOS[args.scenario]
        binary = resolve_bin(args.binary)
        noise, noisy = noise_gate(args.allow_noise)

        print(f"scenario {args.scenario}: {' '.join(argv)}")
        print(f"binary   {binary}")
        print(f"trace    {args.trace}   telemetry off for timing, one extra run with it on for the breakdown")
        print(f"box      loadavg {noise['loadavg1']:.2f} over {noise['cpus']} cpus, {noise['compilers']} compilers")
        print()

        # Warmup, discarded: the first runs of a fresh binary pay JIT and page-cache costs that no later run
        # repeats, and folding them into the median makes every batch look worse than the steady state.
        for _ in range(args.warmup):
            run_once(binary, argv, args.trace, False, fixture_path(args.fixture))

        samples = []
        for i in range(args.n):
            wall, rc, _ = run_once(
            binary, argv, args.trace, args.telemetry == "on", fixture_path(args.fixture))
            if rc != 0:
                sys.exit(
                    f"\nABORT: run {i} exited {rc}. A failing run is fast, so timing it would "
                    f"be meaningless. If you changed packages/, the fixture is stale: reload "
                    f"packages, then `perf-bench fixture save`."
                )
            samples.append(wall)
            print(f"  {wall:8.1f} ms")

        stats = summarise(samples)
        med = stats["median"]
        outliers = [x for x in samples if x > med * OUTLIER_FACTOR]

        # One instrumented run for the phase breakdown. Kept separate from the timing samples on purpose:
        # telemetry is cheap but not free, and a number quoted as wall time should never have been taken with
        # the instrument attached.
        _, _, tel = run_once(binary, argv, args.trace, True, fixture_path(args.fixture))

        print()
        print(f"  median {stats['median']} ms   min {stats['min']}   p90 {stats['p90']}   IQR {stats['iqr']}")
        if outliers:
            print(f"  {len(outliers)} outlier(s) over {OUTLIER_FACTOR}x median: "
                  f"{', '.join(f'{o:.0f}' for o in outliers)}")
        if tel:
            print("  phases (one instrumented run):")
            for k in ("cli.total", "cli.growIfNeeded", "cli.buildState", "cli.execute"):
                if k in tel:
                    print(f"    {k:22s} {tel[k]:>7} ms")
            for k in ("instructions", "packageCalls", "builtinCalls", "framePushes"):
                if k in tel:
                    print(f"    {k:22s} {tel[k]:>7,}")

        record({
            "kind": "run",
            "commit": commit(),
            "config": "Debug",
            "scenario": args.scenario,
            "binary": args.binary,
            "trace": args.trace,
            "wall_ms": stats,
            "telemetry": tel,
            "noise": noise,
            "noisy": noisy,
            "outliers": len(outliers),
        })
        print(f"\nrecorded to {os.path.relpath(SERIES, ROOT)}")


def cmd_check(args):
    materialize_workloads()
    with DevStore():
        """Run every scenario once per binary and report exit codes.

        A scenario that errors returns in a fraction of the time a working one takes, so a broken scenario or
        a mismatched fixture reads as an improvement. Both happened here before this existed: `eval-map1000`
        passed Int64 to `List.range`, which takes Int, and had been failing on both sides of an A/B."""
        binary = resolve_bin(args.binary)
        fixture = fixture_path(args.fixture)
        print(f"binary  {binary}")
        print(f"fixture {os.path.basename(fixture)}\n")
        bad = 0
        for name, argv in SCENARIOS.items():
            ms, rc, _ = run_once(binary, argv, "off", False, fixture)
            status = "ok" if rc == 0 else f"EXIT {rc}"
            if rc != 0:
                bad += 1
            print(f"  {name:18s} {ms:8.1f} ms   {status}")
        print()
        if bad:
            print(f"{bad} scenario(s) failing. Their timings are meaningless; fix before measuring.")
            sys.exit(1)
        print("all scenarios run clean")


def cmd_ab(args):
    materialize_workloads()
    with DevStore():
        if args.scenario not in SCENARIOS:
            sys.exit(f"unknown scenario {args.scenario!r}")
        argv = SCENARIOS[args.scenario]
        a_bin, b_bin = resolve_bin(args.a), resolve_bin(args.b)
        noise, noisy = noise_gate(args.allow_noise)

        print(f"A = {args.a}  ({a_bin})")
        print(f"B = {args.b}  ({b_bin})")
        print(f"scenario {args.scenario}, {args.n} interleaved pairs\n")

        fa, fb = fixture_path(args.fixture_a), fixture_path(args.fixture_b)
        for f, side in ((fa, "A"), (fb, "B")):
            if f is not None and not os.path.exists(f):
                sys.exit(f"no fixture for side {side}: {f}")
        show = lambda f: "the binary's own" if f is None else os.path.basename(f)
        print(f"fixtures  A={show(fa)}  B={show(fb)}\n")

        for _ in range(args.warmup):
            run_once(a_bin, argv, args.trace, False, fa)
            run_once(b_bin, argv, args.trace, False, fb)

        # Interleaved, and the pair is what's compared. Drift across the batch -- a neighbour starting a
        # build halfway through -- moves both halves of a pair together and cancels in the difference, which
        # measuring all of A then all of B would not.
        pairs = []
        for i in range(args.n):
            a_ms, a_rc, _ = run_once(a_bin, argv, args.trace, False, fa)
            b_ms, b_rc, _ = run_once(b_bin, argv, args.trace, False, fb)
            # A failing run is FAST, so timing it silently makes a broken binary look like an optimization.
            # This bit me: after changing stdlib/list.dark the fixture store still held the old content hashes,
            # so the new binary died with FnNotFound in ~0.5 s and the A/B read it as a win.
            if a_rc != 0 or b_rc != 0:
                sys.exit(
                    f"\nABORT: run exited non-zero (A={a_rc}, B={b_rc}) on pair {i}.\n"
                    f"A failing run is fast, so these timings would be meaningless.\n"
                    f"If you changed packages/, the fixture is stale: reload packages, then "
                    f"`perf-bench fixture save`."
                )
            pairs.append((a_ms, b_ms))
            print(f"  A {a_ms:8.1f}   B {b_ms:8.1f}   diff {b_ms - a_ms:+8.1f}")

        a_s = summarise([p[0] for p in pairs])
        b_s = summarise([p[1] for p in pairs])
        diffs = [b - a for a, b in pairs]
        d = summarise(diffs)
        rel = (statistics.median(diffs) / a_s["median"] * 100) if a_s["median"] else 0.0
        wins = sum(1 for x in diffs if x < 0)

        print()
        print(f"  A median {a_s['median']} ms (IQR {a_s['iqr']})")
        print(f"  B median {b_s['median']} ms (IQR {b_s['iqr']})")
        print(f"  paired difference: median {d['median']:+} ms ({rel:+.1f}%), IQR {d['iqr']}")
        print(f"  B faster in {wins}/{len(diffs)} pairs")

        record({
            "kind": "ab",
            "commit": commit(),
            "config": "Debug",
            "scenario": args.scenario,
            "a": args.a, "b": args.b,
            "trace": args.trace,
            "a_ms": a_s, "b_ms": b_s,
            "diff_ms": d, "diff_pct": round(rel, 2),
            "b_faster_pairs": wins, "pairs": len(diffs),
            "noise": noise, "noisy": noisy,
        })
        print(f"\nrecorded to {os.path.relpath(SERIES, ROOT)}")


def main():
    p = argparse.ArgumentParser(prog="perf-bench")
    sub = p.add_subparsers(dest="cmd", required=True)

    sub.add_parser("scenarios")

    fx = sub.add_parser("fixture").add_subparsers(dest="fixcmd", required=True)
    fxs = fx.add_parser("save")
    fxs.add_argument("name", nargs="?", default=None)

    bs = sub.add_parser("bin").add_subparsers(dest="bincmd", required=True)
    bsv = bs.add_parser("save")
    bsv.add_argument("name")

    r = sub.add_parser("run")
    r.add_argument("scenario")
    r.add_argument("-n", type=int, default=10)
    r.add_argument("--warmup", type=int, default=3)
    r.add_argument("--binary", default="current")
    r.add_argument("--trace", choices=["on", "off"], default="off")
    r.add_argument("--allow-noise", action="store_true")
    r.add_argument("--fixture", default=None)
    r.add_argument("--telemetry", choices=["on", "off"], default="off",
                   help="run the timed samples with telemetry on, to measure what the instrument costs")

    ck = sub.add_parser("check")
    ck.add_argument("--binary", default="current")
    ck.add_argument("--fixture", default=None)

    ab = sub.add_parser("ab")
    ab.add_argument("a")
    ab.add_argument("b")
    ab.add_argument("scenario")
    ab.add_argument("-n", type=int, default=15)
    ab.add_argument("--warmup", type=int, default=3)
    ab.add_argument("--trace", choices=["on", "off"], default="off")
    ab.add_argument("--allow-noise", action="store_true")
    ab.add_argument("--fixture-a", default=None, help="store fixture matching binary A")
    ab.add_argument("--fixture-b", default=None, help="store fixture matching binary B")

    args = p.parse_args()

    if args.cmd == "scenarios":
        for k, v in SCENARIOS.items():
            print(f"  {k:16s} dark {' '.join(v)}")
    elif args.cmd == "fixture":
        fixture_save(args.name)
    elif args.cmd == "bin":
        bin_save(args.name)
    elif args.cmd == "run":
        cmd_run(args)
    elif args.cmd == "check":
        cmd_check(args)
    elif args.cmd == "ab":
        cmd_ab(args)


if __name__ == "__main__":
    main()
