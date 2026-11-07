#!/usr/bin/env python3
"""CPU per interpreted call, against the last release, paired. Part of `scripts/perf/gate`.

    python3 scripts/perf/_paired.py <exe> <home>

`<exe>` is the published CLI under test and `<home>` a HOME whose store it has already grown. The
reference is the newest published release, from the GitHub releases API, downloaded once into
rundir/perf/reference/ and checked against the sha256 GitHub lists for it. When the API can't be
reached, it is the release pinned in budget.json (`paired.reference`).

Allocation can't see this. `fib 27` allocates 8 MB on every version while its CPU went up 27%, and
listings got 1.8x slower in the same step. So: three scenarios, a recursive `fib 24`, a lists
workload (map, filter, fold, a dict built and read back) and `ls Darklang.Stdlib.List`, run on both
binaries in alternating order, `rounds` times. Each round
gives a ratio, this binary's CPU over the reference's, and the gate fails when the median ratio is
over `maxRatio`. The two halves of a pair run seconds apart on the same runner, so the runner's
speed and load cancel in the ratio; a median of 11 keeps one slow pair out of it.

A median over the line is measured a second time, and only a second one over it fails. A real
regression fails both; noise would have to land twice. `ls` is short, mostly process startup, so
each of its samples is three runs added up.

The newest release, not a pin, so nobody has to move a pin when one is cut. It is never the build
being measured: a release built from this very commit is skipped. A release published halfway
through a PR's CI moves the reference forward one release, which is what moving the pin did. The
pin in budget.json is the fallback: offline, rate-limited or GitHub down, the gate says so in one
line and measures against it. `GITHUB_TOKEN` (or `GH_TOKEN`), when set, lifts the API's rate
limit. `DARK_PERF_RELEASES_URL` points the lookup elsewhere; a bad one tries the fallback.
"""
import gzip
import hashlib
import json
import os
import re
import shutil
import subprocess
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
LISTS = os.path.join(ROOT, "scripts", "perf", "workloads", "lists.dark")
RELEASES_URL = os.environ.get(
    "DARK_PERF_RELEASES_URL", "https://api.github.com/repos/darklang/dark/releases?per_page=10")
# darklang-alpha-<commit>-linux-x64.gz; the musl build is a different asset.
ASSET = re.compile(r"^darklang-alpha-([0-9a-f]+)-linux-x64\.gz$")

# name -> (CLI arguments, runs per sample, text the output must contain)
SCENARIOS = {
    "fib 24": (["run", FIB], 1, "46368"),
    "lists": (["run", LISTS], 1, "237517500"),
    "ls Darklang.Stdlib.List": (["ls", "Darklang.Stdlib.List"], 3, "map"),
}


def cannot_measure(why):
    """Exit 2, as against 1 for "slower": the gate could not do its job."""
    print(f"perf/gate: {why}")
    sys.exit(2)


def head_commit():
    try:
        return subprocess.run(["git", "rev-parse", "HEAD"], cwd=ROOT, capture_output=True,
                              text=True, check=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return ""


def latest_reference(pinned):
    """The newest published release with a linux-x64 binary, as {tag, asset, sha256}.

    Every release so far is a prerelease, which /releases/latest skips, so this reads the list,
    newest first. Falls back to `pinned`, with one line saying why."""
    headers = {"Accept": "application/vnd.github+json", "User-Agent": "darklang-perf-gate"}
    token = os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")
    if token:
        headers["Authorization"] = f"Bearer {token}"
    releases, err = None, None
    for attempt in range(3):
        try:
            req = urllib.request.Request(RELEASES_URL, headers=headers)
            with urllib.request.urlopen(req, timeout=15) as resp:
                releases = json.load(resp)
            break
        except (OSError, ValueError) as e:
            err = e
            # A 4xx (rate limit, bad URL) is an answer, not a blip: asking again won't change it.
            if 400 <= getattr(e, "code", 0) < 500:
                break
            if attempt < 2:
                time.sleep(2 ** (attempt + 1))
    if releases is None:
        print(f"perf/gate: could not ask GitHub for the newest release ({err}); "
              f"using {pinned['tag']}, pinned in budget.json")
        return pinned
    head = head_commit()
    for rel in releases if isinstance(releases, list) else []:
        if rel.get("draft"):
            continue
        for a in rel.get("assets", []):
            m = ASSET.match(a.get("name", ""))
            digest = a.get("digest") or ""
            if not m or not digest.startswith("sha256:"):
                continue
            if head.startswith(m.group(1)):
                break  # built from the commit being measured: take the one before it
            return {"tag": rel["tag_name"], "asset": a["name"],
                    "sha256": digest.removeprefix("sha256:")}
    print(f"perf/gate: no release on GitHub has a linux-x64 binary to compare with; "
          f"using {pinned['tag']}, pinned in budget.json")
    return pinned


def fetch_reference(ref):
    """The reference release binary, downloaded and checked once, then cached."""
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
        cannot_measure(f"{url} has sha256 {digest}, expected {ref['sha256']}")
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
    ref = latest_reference(cfg["reference"])
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
