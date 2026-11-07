#!/usr/bin/env python3
"""Run a command and print the CPU time and peak memory it used, from `wait4`'s rusage.

    python3 scripts/perf/_rusage.py <command> [args...]

Prints one JSON line, `{"cpu_s": ..., "peak_rss_mb": ...}`, and throws the command's output away.
Exits 2 if the command failed, with the end of its output, because a failing run is fast and small
and would read as an improvement.

CPU time, not wall time: on a shared box wall time moves 2x between runs of one binary. CPU time
moves less, and `_paired.py` interleaves two binaries so what is left moves both sides together.
`perf stat -e instructions:u` would be steadier still, but CI's docker executor doesn't allow
`perf_event_open` and the CI image has no `perf`.
"""
import json
import os
import subprocess
import sys
import tempfile


def run(argv, env=None, cwd=None):
    """(cpu seconds, peak RSS in MB, exit code, output) for one run of `argv`."""
    with tempfile.TemporaryFile() as out:
        proc = subprocess.Popen(
            argv, env=env, cwd=cwd, stdin=subprocess.DEVNULL, stdout=out, stderr=subprocess.STDOUT
        )
        _, status, usage = os.wait4(proc.pid, 0)
        proc.returncode = os.waitstatus_to_exitcode(status)
        out.seek(0)
        text = out.read().decode("utf-8", "replace")
    # `dark run` exits 0 when the script itself fails; see the same check in _bench.py.
    rc = proc.returncode
    if rc == 0 and ("Error when executing Script" in text or "Script error:" in text):
        rc = 1
    # ru_maxrss is in KB on Linux.
    return usage.ru_utime + usage.ru_stime, usage.ru_maxrss / 1024, rc, text


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("usage: _rusage.py <command> [args...]")
    cpu, rss, rc, text = run(sys.argv[1:])
    if rc != 0:
        print(text[-2000:], file=sys.stderr)
        print(f"_rusage.py: {' '.join(sys.argv[1:])} exited {rc}", file=sys.stderr)
        sys.exit(2)
    print(json.dumps({"cpu_s": round(cpu, 3), "peak_rss_mb": round(rss, 1)}))
