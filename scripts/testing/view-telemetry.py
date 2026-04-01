#!/usr/bin/env python3
"""
Telemetry viewer for Darklang CLI traces.
Reads telemetry.jsonl and shows a breakdown of where time went.

Usage:
  ./scripts/view-telemetry.py [path/to/telemetry.jsonl]
  ssh rm2 cat /path/to/telemetry.jsonl | ./scripts/view-telemetry.py -
"""
import json, sys, os
from collections import defaultdict

def load(path):
    if path == "-":
        return [json.loads(l) for l in sys.stdin if l.strip()]
    with open(path) as f:
        return [json.loads(l) for l in f if l.strip()]

def format_ms(ms):
    if ms < 1: return f"{ms*1000:.0f}us"
    if ms < 1000: return f"{ms:.0f}ms"
    return f"{ms/1000:.2f}s"

def bar(ms, max_ms, width=40):
    if max_ms == 0: return ""
    filled = int(ms / max_ms * width)
    return "█" * filled + "░" * (width - filled)

def show_startup(events):
    """Show F#-level startup phases."""
    phases = [e for e in events if e["event"].startswith("cli.") or e["event"].startswith("seed.")]
    if not phases:
        return
    print("\n╔══════════════════════════════════════════════════════════════╗")
    print("║  STARTUP BREAKDOWN                                         ║")
    print("╚══════════════════════════════════════════════════════════════╝\n")
    max_ms = max((e.get("ms", 0) for e in phases), default=1)
    for e in phases:
        ms = e.get("ms", 0)
        name = e["event"]
        ctx = e.get("ctx", {})
        ctx_str = " ".join(f"{k}={v}" for k, v in ctx.items()) if ctx and ctx != {} else ""
        print(f"  {name:<30s} {format_ms(ms):>8s}  {bar(ms, max_ms, 30)}  {ctx_str}")
    print()

def show_interactive(events):
    """Show per-keystroke interactive loop breakdown."""
    loops = [e for e in events if e["event"] == "loopWork"]
    if not loops:
        return
    print("╔══════════════════════════════════════════════════════════════╗")
    print("║  INTERACTIVE LOOP (per keystroke)                           ║")
    print("╚══════════════════════════════════════════════════════════════╝\n")

    hints = [e for e in events if e["event"] == "completionHint"]
    renders = [e for e in events if e["event"] == "render"]
    pre = [e for e in events if e["event"] == "preInput"]
    updates = [e for e in events if e["event"] == "update"]

    def stats(entries):
        ms_list = [e["ms"] for e in entries]
        if not ms_list: return "n/a"
        return f"min={min(ms_list)} avg={sum(ms_list)//len(ms_list)} max={max(ms_list)} ms"

    print(f"  {'Metric':<25s} {'Stats':}")
    print(f"  {'─'*25} {'─'*40}")
    print(f"  {'completionHint':<25s} {stats(hints)}")
    print(f"  {'render':<25s} {stats(renders)}")
    print(f"  {'preInput (total)':<25s} {stats(pre)}")
    print(f"  {'update':<25s} {stats(updates)}")
    print(f"  {'loopWork':<25s} {stats(loops)}")
    print()

    # Show interpreter stats if available
    interp_data = []
    for l in loops:
        ctx = l.get("ctx", {})
        if "interp" in ctx:
            try:
                interp_data.append(json.loads(ctx["interp"]))
            except:
                pass

    if interp_data:
        avg_instrs = sum(d["instructions"] for d in interp_data) // len(interp_data)
        avg_frames = sum(d["framePushes"] for d in interp_data) // len(interp_data)
        avg_builtin = sum(d["builtinCalls"] for d in interp_data) // len(interp_data)
        avg_pkg = sum(d["packageCalls"] for d in interp_data) // len(interp_data)

        print(f"  Interpreter (avg per keystroke):")
        print(f"    Instructions: {avg_instrs:,}")
        print(f"    Frames:       {avg_frames:,}")
        print(f"    Builtin calls:{avg_builtin:,}")
        print(f"    Package calls:{avg_pkg:,}")
        print()

        # Per-builtin timing
        if any("builtinTiming" in d for d in interp_data):
            agg = defaultdict(lambda: {"us": 0, "n": 0})
            count = 0
            for d in interp_data:
                bt = d.get("builtinTiming", {})
                if bt:
                    count += 1
                    for name, stats in bt.items():
                        agg[name]["us"] += stats["us"]
                        agg[name]["n"] += stats["n"]

            if count > 0:
                print(f"  Top builtins by cumulative time:")
                top = sorted(agg.items(), key=lambda x: x[1]["us"], reverse=True)[:10]
                for name, s in top:
                    avg_us = s["us"] // count
                    avg_n = s["n"] // count
                    print(f"    {name:<30s} {avg_us:>7,}us  ({avg_n:>4}x)")
                print()

def show_commands(events):
    """Show command execution times."""
    cmds = [e for e in events if e["event"] == "commandExec"]
    if not cmds:
        return
    print(f"  Commands executed:")
    for c in cmds:
        print(f"    {c['ctx'].get('cmd', '?'):<20s} {format_ms(c['ms'])}")
    print()

def main():
    # Find telemetry file
    if len(sys.argv) > 1:
        path = sys.argv[1]
    else:
        candidates = [
            "rundir/logs/telemetry.jsonl",
            os.path.expanduser("~/.darklang/logs/telemetry.jsonl"),
        ]
        path = next((p for p in candidates if os.path.exists(p)), None)
        if not path:
            print("No telemetry file found. Specify path as argument.")
            sys.exit(1)

    events = load(path)
    if not events:
        print("No telemetry events found.")
        sys.exit(0)

    print(f"\n  Telemetry: {path} ({len(events)} events)")
    print(f"  Time range: {events[0].get('wall', '?')} -> {events[-1].get('wall', '?')}")

    show_startup(events)
    show_interactive(events)
    show_commands(events)

if __name__ == "__main__":
    main()
