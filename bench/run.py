#!/usr/bin/env python3
"""Bench runner: drive `claude -p` per (task, language, trial), grade the
model's self-reported PASS, and append cost/token/pass-fail/time metrics to
bench/results.csv.

Usage:
    bench/run.py --langs python --trials 1
    bench/run.py --langs dark,python,ts --trials 5
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import functools
import json
import os
import re
import subprocess
import sys
import time
import uuid
from pathlib import Path

HARNESS_VERSION = "0.1"
BENCH_ROOT = Path(__file__).resolve().parent
REPO_ROOT  = BENCH_ROOT.parent
TASKS_DIR  = BENCH_ROOT / "tasks"
LANG_DIR   = BENCH_ROOT / "lang"
RUNS_DIR   = BENCH_ROOT / "runs"
RESULTS    = BENCH_ROOT / "results.csv"
REPORT     = BENCH_ROOT / "report.md"

CSV_FIELDS = [
    "run_id", "timestamp", "task", "lang", "trial",
    "passed", "num_turns", "wall_ms",
    "input_tokens", "output_tokens",
    "cache_read_tokens", "cache_creation_tokens",
    "cost_usd", "model", "session_id",
    "harness_version", "git_sha", "claude_version",
    "error",
]

DEFAULT_TIMEOUT_SEC = 900   # 15 min per trial
DEFAULT_BUDGET_USD  = 5.0
DEFAULT_LANGS       = "python,dark"
DEFAULT_MODEL       = "claude-opus-4-6"

# Per-language structural rules. Inlined into every prompt so the model
# knows where the seed shim expects the implementation. The Dark rules
# are non-negotiable (the shim hardcodes the branch + Bench.<NS>.<cmd>
# naming); python/ts only need to know the filename the shim runs.
LANG_RULES = {
    "python": (
        "Build with Python 3 stdlib only. Put your implementation in `main.py` — "
        "the seed `./run` runs `python3 main.py \"$@\"`."
    ),
    "ts": (
        "Build with Bun. Put your implementation in `main.ts` — the seed `./run` "
        "runs `bun main.ts \"$@\"`."
    ),
    "dark": (
        "Build using `$DARK_REPO/scripts/run-cli`. Read `./scripts/run-cli docs for-ai` "
        "for syntax and workflow.\n"
        "Bench rules (the seed shim depends on these):\n"
        "- `cd \"$DARK_REPO\"` before invoking `./scripts/run-cli` (your cwd is the "
        "workspace, not the repo root).\n"
        "- Pass `--branch \"$BENCH_DARK_BRANCH\"` on every `run-cli` invocation.\n"
        "- Build functions named `Bench.{{namespace}}.<subcommand>` — one per "
        "subcommand in the spec; the shim dispatches by name.\n"
        "- Read/write persisted state under `$BENCH_STATE_DIR`."
    ),
}


def shell(cmd: list[str], **kw) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, capture_output=True, text=True, **kw)


@functools.cache
def git_sha() -> str:
    try:
        return shell(["git", "-C", str(REPO_ROOT), "rev-parse", "--short", "HEAD"]).stdout.strip()
    except FileNotFoundError:
        return ""


@functools.cache
def claude_version() -> str:
    try:
        return shell(["claude", "--version"]).stdout.strip()
    except FileNotFoundError:
        return ""


def create_dark_branch(branch_name: str) -> None:
    r = shell(
        [str(REPO_ROOT / "scripts" / "run-cli"), "branch", "create", branch_name],
        cwd=str(REPO_ROOT),
    )
    if r.returncode != 0:
        raise RuntimeError(f"branch create failed: {r.stderr or r.stdout}")


def task_namespace(task: str) -> str:
    """kebab-case task name → PascalCase Darklang namespace component.

    e.g. "url-shortener-cli" → "UrlShortenerCli". The shim prepends "Bench."
    """
    return "".join(part[:1].upper() + part[1:] for part in task.split("-") if part)


def setup_workspace(workspace: Path, lang: str, namespace: str) -> None:
    """Drop the lang-default ./run shim into the workspace."""
    workspace.mkdir(parents=True, exist_ok=True)
    shim_src = (LANG_DIR / lang / "run").read_text()
    if lang == "dark":
        shim_src = shim_src.replace("{{namespace}}", namespace)
    shim_dst = workspace / "run"
    shim_dst.write_text(shim_src)
    shim_dst.chmod(0o755)


def build_prompt(task_md: str, lang: str, namespace: str) -> str:
    rules = LANG_RULES[lang].replace("{{namespace}}", namespace)
    where = ("All implementation work happens in the Darklang package tree on "
             "the branch named in $BENCH_DARK_BRANCH."
             if lang == "dark"
             else "Build the implementation in the current working directory.")
    friction = (
        "Before the final PASS line, write a section titled `## Dark friction` "
        "listing 3–5 specific things that slowed you down or surprised you while "
        "implementing this in Darklang (concrete: name the function, feature, or "
        "error message; skip generic complaints). Omit the section only if "
        "nothing notable came up.\n\n"
        if lang == "dark"
        else ""
    )
    return (
        f"{task_md.rstrip()}\n\n"
        f"---\n\n"
        f"Implement in **{lang}**. {where}\n\n"
        f"{rules}\n\n"
        f"When you finish, verify the implementation by exercising `./run`. "
        f"{friction}"
        f"Print `PASS` on the last line of your final reply if it meets every "
        f"requirement, otherwise print a short reason."
    )


def collect_dark_friction(rows: list[dict]) -> list[tuple[str, int, str]]:
    """For each dark trial, pull the `## Dark friction` section out of the
    model's final reply. Returns (task, trial, body) tuples."""
    out: list[tuple[str, int, str]] = []
    for r in rows:
        if r["lang"] != "dark":
            continue
        cr = RUNS_DIR / r["run_id"] / "claude_result.json"
        if not cr.is_file():
            continue
        try:
            data = json.loads(cr.read_text())
        except json.JSONDecodeError:
            continue
        text = data.get("result") or ""
        m = re.search(
            r"##\s*Dark friction\s*\n(.+?)(?=\n##|\nPASS\b|\Z)",
            text, re.DOTALL | re.IGNORECASE,
        )
        if m and m.group(1).strip():
            out.append((r["task"], r["trial"], m.group(1).strip()))
    return out


def grade(claude_result: dict) -> tuple[bool, str]:
    """Read claude's final message and look for PASS / a failure reason."""
    text = (claude_result.get("result") or "").strip()
    last = text.splitlines()[-1].strip() if text else ""
    if last.upper().startswith("PASS"):
        return True, ""
    return False, last[:200]


def run_claude(prompt: str, workspace: Path, env: dict, timeout_s: int, budget: float, model: str) -> tuple[dict, str, int]:
    cmd = [
        "claude", "-p",
        "--output-format", "json",
        "--permission-mode", "bypassPermissions",
        "--add-dir", str(REPO_ROOT),
        "--max-budget-usd", str(budget),
        "--model", model,
    ]
    started = time.monotonic()
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(workspace),
            env=env,
            input=prompt,
            capture_output=True,
            text=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired as e:
        wall_ms = int((time.monotonic() - started) * 1000)
        partial = e.stdout or b""
        if isinstance(partial, bytes):
            partial = partial.decode(errors="replace")
        return ({"_error": "timeout", "_wall_ms": wall_ms}, partial, -1)
    wall_ms = int((time.monotonic() - started) * 1000)
    raw = proc.stdout
    parsed: dict = {}
    try:
        parsed = json.loads(raw) if raw.strip() else {}
    except json.JSONDecodeError:
        parsed = {"_error": "json-parse"}
    parsed["_wall_ms"]  = wall_ms
    parsed["_stderr_tail"] = (proc.stderr or "")[-2000:]
    return parsed, raw, proc.returncode


def metrics_from_result(r: dict, requested_model: str) -> dict:
    usage = r.get("usage") or {}
    return {
        "num_turns":             r.get("num_turns"),
        "input_tokens":          usage.get("input_tokens"),
        "output_tokens":         usage.get("output_tokens"),
        "cache_read_tokens":     usage.get("cache_read_input_tokens"),
        "cache_creation_tokens": usage.get("cache_creation_input_tokens"),
        "cost_usd":              r.get("total_cost_usd"),
        "model":                 requested_model,
        "session_id":            r.get("session_id"),
    }


def append_csv(row: dict) -> None:
    new = not RESULTS.exists()
    RESULTS.parent.mkdir(parents=True, exist_ok=True)
    with RESULTS.open("a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=CSV_FIELDS)
        if new:
            w.writeheader()
        w.writerow({k: row.get(k, "") for k in CSV_FIELDS})


def do_trial(task: str, lang: str, trial: int, args) -> dict:
    task_md_path = TASKS_DIR / f"{task}.md"
    if not task_md_path.is_file():
        raise SystemExit(f"missing task file: {task_md_path}")
    if not (LANG_DIR / lang / "run").is_file():
        raise SystemExit(f"unknown lang '{lang}' (no bench/lang/{lang}/run)")

    task_md = task_md_path.read_text()
    namespace = task_namespace(task)

    ts        = dt.datetime.now().strftime("%Y%m%dT%H%M%S")
    run_id    = f"{ts}_{task}_{lang}_t{trial}_{uuid.uuid4().hex[:6]}"
    run_dir   = RUNS_DIR / run_id
    workspace = run_dir / "workspace"
    run_dir.mkdir(parents=True)

    setup_workspace(workspace, lang, namespace)

    env = os.environ.copy()
    if lang == "dark":
        branch_name = f"bench-{task}-{ts}-t{trial}".replace("_", "-")
        create_dark_branch(branch_name)
        env["BENCH_DARK_BRANCH"] = branch_name
        env["DARK_REPO"]         = str(REPO_ROOT)

    prompt = build_prompt(task_md, lang, namespace)
    (run_dir / "prompt.md").write_text(prompt)

    print(f"[{run_id}] starting claude model={args.model} (timeout={args.timeout}s, budget=${args.budget})...", flush=True)
    result, raw, rc = run_claude(prompt, workspace, env, args.timeout, args.budget, args.model)
    (run_dir / "claude_result.json").write_text(raw or "")

    err = result.get("_error", "")
    if rc != 0 and not err:
        err = f"claude-exit-{rc}"
    passed, fail_reason = (False, "")
    if not err:
        passed, fail_reason = grade(result)

    m = metrics_from_result(result, args.model)
    metrics = {
        "run_id":          run_id,
        "timestamp":       dt.datetime.now().isoformat(timespec="seconds"),
        "task":            task,
        "lang":            lang,
        "trial":           trial,
        "passed":          passed,
        "wall_ms":         result.get("_wall_ms"),
        "harness_version": HARNESS_VERSION,
        "git_sha":         git_sha(),
        "claude_version":  claude_version(),
        "error":           err or fail_reason,
        **m,
    }
    (run_dir / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n")
    append_csv(metrics)

    status = "PASS" if passed else ("ERR" if err else "FAIL")
    cost = metrics.get("cost_usd")
    cost_s = f"${cost:.3f}" if isinstance(cost, (int, float)) else "$?"
    print(f"[{run_id}] {status}  turns={metrics.get('num_turns')}  "
          f"wall={metrics.get('wall_ms')}ms  {cost_s}", flush=True)
    return metrics


def write_report(rows: list[dict]) -> bool:
    """Write bench/report.md with the dark friction notes from this run.
    Returns True if a report was written, False if there was nothing to say."""
    sections = collect_dark_friction(rows)
    if not sections:
        return False
    out: list[str] = []
    out.append("# Dark friction")
    out.append("")
    out.append(f"Generated: {dt.datetime.now().isoformat(timespec='seconds')} "
               f"(git {git_sha() or '?'}, {claude_version() or 'claude ?'})")
    for task, trial, body in sections:
        out.append("")
        out.append(f"## {task} (trial {trial})")
        out.append("")
        out.append(body)
    REPORT.write_text("\n".join(out) + "\n")
    return True


def print_summary(rows: list[dict]) -> None:
    if not rows:
        return
    by_task: dict[str, list[dict]] = {}
    for r in rows:
        by_task.setdefault(r["task"], []).append(r)
    for task in sorted(by_task):
        print()
        print(task)
        print(f"  {'lang':<8} {'pass':>5} {'turns':>6} {'wall_s':>8} {'cost':>8}")
        by_lang: dict[str, list[dict]] = {}
        for r in by_task[task]:
            by_lang.setdefault(r["lang"], []).append(r)
        for lang, rs in by_lang.items():
            passed     = sum(1 for r in rs if r["passed"])
            avg_turns  = sum((r.get("num_turns") or 0)  for r in rs) / len(rs)
            avg_wall_s = sum((r.get("wall_ms")   or 0)  for r in rs) / len(rs) / 1000
            avg_cost   = sum((r.get("cost_usd")  or 0.0) for r in rs) / len(rs)
            print(f"  {lang:<8} {passed}/{len(rs):<3} {avg_turns:>6.1f} "
                  f"{avg_wall_s:>8.1f} ${avg_cost:>6.3f}")


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--task",    default=None,
                   help="limit to one task (default: every .md under bench/tasks/)")
    p.add_argument("--langs",   default=DEFAULT_LANGS, help="comma-separated: dark,python,ts")
    p.add_argument("--trials",  type=int,   default=1)
    p.add_argument("--timeout", type=int,   default=DEFAULT_TIMEOUT_SEC)
    p.add_argument("--budget",  type=float, default=DEFAULT_BUDGET_USD)
    p.add_argument("--model",   default=DEFAULT_MODEL,
                   help="model name passed to `claude -p --model`")
    args = p.parse_args()

    if args.task:
        tasks = [args.task]
    else:
        tasks = sorted(t.stem for t in TASKS_DIR.glob("*.md"))
        if not tasks:
            raise SystemExit(f"no tasks found under {TASKS_DIR}")

    langs = [l.strip() for l in args.langs.split(",") if l.strip()]
    rows: list[dict] = []
    for task in tasks:
        for lang in langs:
            for trial in range(1, args.trials + 1):
                rows.append(do_trial(task, lang, trial, args))
    print_summary(rows)
    if write_report(rows):
        print(f"\nfriction report: {REPORT}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
