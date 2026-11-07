#!/usr/bin/env python3
"""CPU per interpreted call, against the last release, paired. Part of `scripts/perf/gate`.

    python3 scripts/perf/_paired.py <exe> <home>

`<exe>` is the published CLI under test and `<home>` a HOME whose store it has already grown. The
reference is the release pinned in budget.json (`paired.reference`), downloaded once into
rundir/perf/reference/ and checked against its sha256.

Allocation can't see this. `fib 27` allocates 8 MB on every version while its CPU went up 27%, and
listings got 1.8x slower in the same step. So: two scenarios, a recursive `fib 24` and
`ls Darklang.Stdlib.List`, run on both binaries in alternating order, `rounds` times. Each round
gives a ratio, this binary's CPU over the reference's, and the gate fails when the median ratio is
over `maxRatio`. The two halves of a pair run seconds apart on the same runner, so the runner's
speed and load cancel in the ratio; a median of 11 keeps one slow pair out of it.

A median over the line is measured a second time, and only a second one over it fails. A real
regression fails both; noise would have to land twice. `ls` is short, mostly process startup, so
each of its samples is three runs added up.

The reference is a pinned release rather than "the newest one", so a release published halfway
through a PR's CI can't change what the PR is measured against. Move the pin to a newer release
when one is cut. Never move it to the build being measured: that is how the allocation budget came
to be raised in the same PRs that regressed it.
"""
import gzip
import hashlib
import json
import os
import shutil
import statistics
import sys
import tempfile
import time
import urllib.request

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _rusage import run  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
BUDGET_FILE = os.path.join(ROOT, "scripts", "perf", "budget.json")
CACHE = os.path.join(ROOT, "rundir", "perf", "reference")
FIB = os.path.join(ROOT, "scripts", "perf", "workloads", "fib.dark")

# name -> (CLI arguments, runs per sample, text the output must contain)
SCENARIOS = {
    "fib 24": (["run", FIB], 1, "46368"),
    "ls Darklang.Stdlib.List": (["ls", "Darklang.Stdlib.List"], 3, "map"),
}


def cannot_measure(why):
    """Exit 2, as against 1 for "slower": the gate could not do its job."""
    print(f"perf/gate: {why}")
    sys.exit(2)


def fetch_reference(ref):
    """The pinned release binary, downloaded and checked once, then cached."""
    exe = os.path.join(CACHE, ref["tag"], ref["asset"].removesuffix(".gz"))
    if os.path.exists(exe):
        return exe
    url = f"https://github.com/darklang/dark/releases/download/{ref['tag']}/{ref['asset']}"
    os.makedirs(os.path.dirname(exe), exist_ok=True)
    data = None
    for attempt in range(5):
        try:
            with urllib.request.urlopen(url, timeout=120) as resp:
                data = resp.read()
            break
        except OSError as e:
            print(f"  download attempt {attempt + 1} failed: {e}")
            time.sleep(2 ** (attempt + 1))
    if data is None:
        cannot_measure(f"could not download the reference release from {url}")
    digest = hashlib.sha256(data).hexdigest()
    if digest != ref["sha256"]:
        cannot_measure(f"{url} has sha256 {digest}, budget.json pins {ref['sha256']}")
    tmp = exe + ".partial"
    with open(tmp, "wb") as f:
        f.write(gzip.decompress(data))
    os.chmod(tmp, 0o755)
    os.replace(tmp, exe)
    return exe


def env_for(home):
    env = dict(os.environ, HOME=home, DARK_CONFIG_TRACE_DETAIL="off")
    env.pop("DARK_TELEMETRY", None)
    return env


def cpu_of(exe, home, args, runs, expect):
    total = 0.0
    for _ in range(runs):
        cpu, _, rc, text = run([exe, "--no-trace"] + args, env_for(home), ROOT)
        # A failing run is fast; never count one.
        if rc != 0 or expect not in text:
            print(text[-2000:])
            cannot_measure(f"`{os.path.basename(exe)} {' '.join(args)}` exited {rc} or did not "
                           f"print {expect!r}; its CPU time would mean nothing.")
        total += cpu
    return total


def batch(pairs, rounds, names):
    """name -> list of per-round ratios, new / reference."""
    ratios = {n: [] for n in names}
    for i in range(rounds):
        for name in names:
            args, runs, expect = SCENARIOS[name]
            # Alternate which side goes first, so going first or second can't favour one side.
            order = pairs if i % 2 == 0 else pairs[::-1]
            cpu = {side: cpu_of(exe, home, args, runs, expect) for side, exe, home in order}
            ratios[name].append(cpu["new"] / cpu["reference"])
    return ratios


def main():
    if len(sys.argv) != 3:
        sys.exit("usage: _paired.py <exe> <home>")
    exe, home = os.path.abspath(sys.argv[1]), sys.argv[2]
    cfg = json.load(open(BUDGET_FILE))["paired"]
    ref = cfg["reference"]
    limit, rounds = cfg["maxRatio"], cfg["rounds"]

    ref_exe = fetch_reference(ref)
    ref_home = tempfile.mkdtemp(prefix="perf-gate-reference-")
    try:
        # The reference grows its own store first, and that run is not measured.
        cpu_of(ref_exe, ref_home, ["eval", "1 + 2"], 1, "3")
        pairs = [("reference", ref_exe, ref_home), ("new", exe, home)]

        def report(ratios, label):
            over = []
            for name, rs in ratios.items():
                med = statistics.median(rs)
                print(f"{name:26s} {med:.3f}x {ref['tag']}  (pairs {min(rs):.2f}-{max(rs):.2f}){label}")
                if med > limit:
                    over.append(name)
            return over

        print(f"CPU against {ref['tag']}, median of {rounds} pairs, limit {limit}x")
        over = report(batch(pairs, rounds, list(SCENARIOS)), "")
        if over:
            print("over the limit; measuring those again")
            over = report(batch(pairs, rounds, over), "  (again)")
        if over:
            print(f"FAIL: {', '.join(over)} over {limit}x {ref['tag']} twice running.")
            print("This is CPU per call, which allocation does not see. Profile it before")
            print("raising maxRatio, and raise it only in a commit that says why.")
            sys.exit(1)
    finally:
        shutil.rmtree(ref_home, ignore_errors=True)


if __name__ == "__main__":
    main()
