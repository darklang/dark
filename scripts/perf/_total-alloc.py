#!/usr/bin/env python3
"""Bytes a run allocated, from the `gc.stats` event it wrote to telemetry.

The one place this is read. `gate` and `suite` each had their own copy of this parse, and they
disagreed: one took the FIRST `gc.stats` event and the other the LAST. A run writes exactly one
today, so they agreed by luck rather than by design, and a second event would have made two tools
report different numbers for the same run with nothing saying so.

So: this takes the last one, and SAYS when there was more than one, rather than quietly picking.

    python3 scripts/perf/_total-alloc.py <telemetry.jsonl>

Prints the byte count. Exits 2 if there is no such event, which means the run did not finish (or
did not have DARK_TELEMETRY=1), not that it allocated nothing.
"""
import json
import sys

if len(sys.argv) != 2:
    sys.exit("usage: _total-alloc.py <telemetry.jsonl>")

path = sys.argv[1]
found = []
try:
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue  # a partially written last line, mid-flush
            if record.get("event") == "gc.stats":
                found.append(int(record["ctx"]["totalAllocatedBytes"]))
except FileNotFoundError:
    sys.exit(f"_total-alloc.py: no telemetry at {path} -- was DARK_TELEMETRY=1 set?")

if not found:
    sys.exit(f"_total-alloc.py: no gc.stats event in {path} -- did the run finish?")
if len(found) > 1:
    print(
        f"_total-alloc.py: {len(found)} gc.stats events, using the last",
        file=sys.stderr,
    )
print(found[-1])
