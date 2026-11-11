#!/usr/bin/env python3
"""The interactive CLI's first frame and one keypress. Part of `scripts/perf/gate`.

    python3 scripts/perf/_ui.py <exe> <keys> <out.json>

Runs workloads/ui/workbench.dark in the current HOME's store with telemetry on, which totals the
VM instructions of every VM in the process at exit, and the bytes it allocated. Both repeat run to
run, so they are what the gate budgets; wall time is reported beside them and gates nothing.
Instructions alone would miss most of a keypress: it spends most of its time in builtins (the
store, rendering), which run no VM instructions but do allocate.

  firstFrame  the run at 0: process start, the store, the script, and the workbench's first
              frame built and painted to a string
  keypress    the runs at 1 and 1+keys, differenced and divided by keys: one key handled, the
              next frame built and diffed, with startup and the setup cancelled out
"""
import json
import os
import re
import subprocess
import sys
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
# PERF_WORKLOADS: another copy of workloads/, which measure-release points at a release's own.
WORKLOAD = os.path.join(os.environ.get("PERF_WORKLOADS") or os.path.join(ROOT, "scripts", "perf", "workloads"), "ui", "workbench.dark")


def run(exe, iters):
    """({instructions, allocated}, wall ms, output) for one run at PERF_ITERS=iters."""
    telemetry = os.path.join(os.environ["HOME"], ".darklang", "logs", "telemetry.jsonl")
    if os.path.exists(telemetry):
        os.remove(telemetry)
    env = dict(os.environ, PERF_ITERS=str(iters), DARK_TELEMETRY="1", DARK_CONFIG_TRACE_DETAIL="off")
    t0 = time.monotonic()
    proc = subprocess.run([exe, "--no-trace", "run", WORKLOAD], env=env, cwd=ROOT,
                          stdin=subprocess.DEVNULL, capture_output=True, text=True)
    ms = (time.monotonic() - t0) * 1000
    out = proc.stdout + proc.stderr
    if proc.returncode != 0 or not re.search(r"first frame|keys=", out) or "Script error" in out:
        print(out[-2000:])
        sys.exit(f"perf/gate: {os.path.basename(WORKLOAD)} at {iters} failed; its numbers would mean nothing")
    got = {}
    with open(telemetry) as f:
        for line in f:
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue
            if record.get("event") == "vm.stats":
                got["instructions"] = int(record["ctx"]["instructions"])
            elif record.get("event") == "gc.stats":
                got["allocated"] = int(record["ctx"]["totalAllocatedBytes"])
    if len(got) != 2:
        sys.exit(f"perf/gate: no vm.stats or gc.stats in {telemetry}")
    return got, ms, out


def main():
    if len(sys.argv) != 4:
        sys.exit("usage: _ui.py <exe> <keys> <out.json>")
    exe, keys, out_path = sys.argv[1], int(sys.argv[2]), sys.argv[3]
    # The workload runs as a guest script, so it needs what the real `dark` has as trusted CLI
    # code: the workbench calls `native` builtins once a key is pressed.
    for rule in (["env", "read", "PERF_ITERS"], ["native"]):
        subprocess.run([exe, "permissions", "allow", *rule],
                       stdin=subprocess.DEVNULL, capture_output=True)
    first, first_ms, _ = run(exe, 0)
    one, _, one_out = run(exe, 1)
    many, _, many_out = run(exe, 1 + keys)
    elapsed = lambda out: int(re.search(r"elapsed_ms=(\d+)", out).group(1))
    keypress = {k: (many[k] - one[k]) // keys for k in many}
    keypress["ms"] = round((elapsed(many_out) - elapsed(one_out)) / keys)
    with open(out_path, "w") as f:
        json.dump({"firstFrame": dict(first, ms=round(first_ms)), "keypress": keypress}, f)


if __name__ == "__main__":
    main()
