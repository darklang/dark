#!/usr/bin/env python3.12
"""The compiler coverage report: which package fns compile, which don't and why, and
of those that compile, which provably agree with the interpreter.

Runs in the container (scripts/compiler/report is the entry point). Two sweeps, both
through the flag-on CLI binary's `compilerCoverageSweep` / `compilerEquivSweep`
builtins, in chunks, in parallel for the compile-only sweep, with a time budget per
chunk. A chunk that overruns is split in two and both halves re-queued, down to one
fn, which is then recorded as a timeout; that is how a fn that hangs the compiler
costs minutes rather than the whole run.

Writes docs/compiler/coverage/<date>.md and .tsv, and diffs against the newest
earlier report in that directory.
"""

import argparse
import collections
import datetime
import glob
import os
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
EXE = os.path.join(ROOT, "backend/Build/out/Cli/Debug/net10.0/Cli")
HASH = re.compile(r"^[0-9a-f]{64}$")


def die(msg):
    print(msg, file=sys.stderr)
    sys.exit(2)


# ---------------------------------------------------------------------------
# What to sweep
# ---------------------------------------------------------------------------


def package_fns(db):
    """name -> hash for every listed package fn, plus the distinct hashes."""
    con = sqlite3.connect(db)
    rows = con.execute(
        "SELECT owner || '.' || modules || '.' || name, item_hash FROM locations "
        "WHERE item_type = 'fn' AND unlisted_at IS NULL ORDER BY 1"
    ).fetchall()
    con.close()
    names = collections.defaultdict(list)
    for n, h in rows:
        names[h].append(n)
    return names


def fn_dependencies(db):
    """hash -> set of fn hashes it calls directly (from the store's projection)."""
    con = sqlite3.connect(db)
    rows = con.execute(
        "SELECT item_hash, depends_on_hash FROM package_dependencies WHERE depends_on_item_type = 'fn'"
    ).fetchall()
    con.close()
    deps = collections.defaultdict(set)
    for a, b in rows:
        if a != b:
            deps[a].add(b)
    return deps


# ---------------------------------------------------------------------------
# Running a sweep builtin over a chunk, with bisection
# ---------------------------------------------------------------------------


def run_chunk(builtin, hashes, timeout):
    """(rc, stdout) for one process over these hashes. rc 124 means timed out.

    Each process gets its own DARK_RPC_DIR: the equivalence harness talks to its
    compiled binaries through files there, so this is what lets several run at once."""
    lit = "[" + ",".join(f'"{h}"' for h in hashes) + "]"
    rpc = tempfile.mkdtemp(prefix="dark-rpc-")
    env = {**os.environ,
           "DARK_CONFIG_RUNDIR": os.environ.get("DARK_CONFIG_RUNDIR", os.path.join(ROOT, "rundir")),
           "DARK_RPC_DIR": rpc}
    try:
        # cwd is the throwaway dir too: the equivalence harness RUNS fns in the
        # interpreter with synthesized arguments, and one of them wrote a 51 MB
        # store copy named "hello" into the repo root before this was here.
        p = subprocess.run([EXE, "eval", f"Builtin.{builtin} {lit}"],
                           capture_output=True, text=True, timeout=timeout, env=env, cwd=rpc)
        return p.returncode, p.stdout
    except subprocess.TimeoutExpired:
        return 124, ""
    finally:
        shutil.rmtree(rpc, ignore_errors=True)


def sweep(builtin, hashes, chunk, timeout, parallel, parse, log):
    """Run `builtin` over all hashes. `parse(hashes, stdout) -> {hash: result}`."""
    results = {}
    work = [hashes[i:i + chunk] for i in range(0, len(hashes), chunk)]

    def one(hs):
        rc, out = run_chunk(builtin, hs, timeout)
        return hs, rc, out

    with ThreadPoolExecutor(max_workers=parallel) as pool:
        pending = [pool.submit(one, hs) for hs in work]
        while pending:
            done = pending.pop(0)
            hs, rc, out = done.result()
            if rc == 124:
                if len(hs) == 1:
                    results[hs[0]] = "timeout|the compiler ran past the %ds budget" % timeout
                    log(f"  timeout: {hs[0][:10]}")
                else:
                    half = (len(hs) + 1) // 2
                    log(f"  {len(hs)} fns over budget, splitting")
                    pending.append(pool.submit(one, hs[:half]))
                    pending.append(pool.submit(one, hs[half:]))
            else:
                got = parse(hs, out)
                results.update(got)
                log(f"  {len(hs)} fns done")
    return results


def parse_coverage(hashes, out):
    """One `True|...`/`False|...` line per hash, in order; a line that starts with
    neither continues the previous record."""
    recs = []
    for line in out.split("\n"):
        if line.startswith("True|") or line.startswith("False|"):
            recs.append(line)
        elif recs and line.strip():
            recs[-1] += " " + line.strip()
    if len(recs) != len(hashes):
        return {h: "False|<sweep output did not line up: %d records for %d fns>" % (len(recs), len(hashes)) for h in hashes}
    return dict(zip(hashes, recs))


REC = re.compile(r"^([0-9a-f]{64})\t(.*)$")


EQUIV_META = {}  # hash -> {"args":..., "interp_ms":..., "compiled_ms":...}


def parse_equiv(hashes, out):
    """`<hash>\\t<result>` records; continuation lines belong to the last record. A
    `meta|args=..|interp_ms=..|compiled_ms=..` record precedes a verdict for the same
    hash and is kept apart."""
    got = {}
    cur = None
    for line in out.split("\n"):
        m = REC.match(line)
        if m:
            h, val = m.group(1), m.group(2)
            if val.startswith("meta|"):
                fields = dict(part.split("=", 1) for part in val[5:].split("|") if "=" in part)
                EQUIV_META[h] = fields
                cur = None
            else:
                cur = h
                got[cur] = val
        elif cur and line.strip():
            got[cur] += " " + line.strip()
    for h in hashes:
        got.setdefault(h, "missing|no record in the sweep output")
    return got


# ---------------------------------------------------------------------------
# Classifying
# ---------------------------------------------------------------------------


def norm(d):
    d = re.sub(r"[0-9a-f]{64}", "<h>", d)
    d = re.sub(r"`[^`]*`", "`_`", d)
    d = re.sub(r"\d+ bytes", "<n> bytes", d)
    return d


def category(detail):
    m = re.match(r"unsupported-builtin: ([A-Za-z0-9]+)(_v\d+)?(: .*)?", detail)
    if m:
        if "unmarshalable-return" in (m.group(3) or ""):
            return "builtin routed, return type unmarshalable"
        return "builtin not routed"
    m = re.match(r"unsupported-([a-z-]+):", detail)
    if m:
        return "unsupported-" + m.group(1)
    if detail.startswith("ANF conversion error"):
        return "compiler: ANF"
    if detail.startswith("timeout"):
        return "compiler: timeout"
    if any(k in detail for k in ("expects", "Type mismatch", "Failed to create record", "Unknown record type")):
        return "compiler: type error"
    return "other"


def equiv_verdict(r):
    return r.split("|", 1)[0].strip() or "?"


# ---------------------------------------------------------------------------
# The report
# ---------------------------------------------------------------------------


def top_ns(name):
    return ".".join(name.split(".")[:2])


def write_report(out_md, out_tsv, names, cov, eq, previous, when, budget):
    total = len(cov)
    compiles = {h for h, r in cov.items() if r.startswith("True|")}
    lines = []
    w = lines.append
    w(f"# Compiler coverage, {when}")
    w("")
    w("Which package fns the native compiler compiles, which it doesn't and why, and of")
    w("those that compile, which provably agree with the interpreter on synthesized")
    w("arguments. Generated by `scripts/compiler/report`; the `.tsv` beside this file has")
    w("every fn.")
    w("")
    w("\"Compiles\" means the bridge lowered the fn and its whole closure and the compiler")
    w("emitted an ELF. \"Match\" means the compiled binary and the interpreter produced the")
    w("same wire bytes for the same synthesized arguments; \"unprovable\"/\"noargs\" means the")
    w("harness could not build arguments (a function-typed or custom-typed parameter),")
    w("which says nothing about correctness either way. Authored cases (CompilerCases) will")
    w("replace synthesized arguments as they land.")
    w("")
    w("---")
    w("")
    w("## Headline")
    w("")
    nmatch = sum(1 for r in eq.values() if equiv_verdict(r) == "match")
    ndiff = sum(1 for r in eq.values() if equiv_verdict(r) == "DIFF")
    nattempt = nmatch + ndiff
    w(f"    package fns            {total}")
    w(f"    compile                {len(compiles)}  ({100 * len(compiles) / max(total, 1):.1f}%)")
    if eq:
        w(f"    proven equal           {nmatch}  of {nattempt} where arguments could be synthesized  ({100 * nmatch / max(nattempt, 1):.1f}%)")
        w(f"    DIFF (miscompile?)     {ndiff}")
        w(f"    could not prove        {len(eq) - nattempt}  (see the equivalence section)")
    if previous:
        pc, pt, _ = previous
        w(f"    previous report        {pc} of {pt} compiled ({100 * pc / max(pt, 1):.1f}%)")
    w("")
    w("## By namespace")
    w("")
    ns = collections.defaultdict(lambda: [0, 0, 0, 0])  # compile, total, match, attempted
    for h, r in cov.items():
        for n in names.get(h, ["?"]):
            k = top_ns(n)
            ns[k][1] += 1
            ns[k][0] += r.startswith("True|")
            if h in eq:
                v = equiv_verdict(eq[h])
                ns[k][2] += v == "match"
                ns[k][3] += v in ("match", "DIFF")
    w("    compile / fns          match / attempted   namespace")
    for k, (a, b, c, d) in sorted(ns.items(), key=lambda kv: -kv[1][1]):
        w(f"    {a:5d} / {b:5d}  {100 * a / b:5.1f}%   {c:5d} / {d:5d}          {k}")
    w("")
    w("## Why the rest don't compile")
    w("")
    w("By the FIRST blocker the bridge or compiler reported for the fn; a fn with two")
    w("problems is counted under the one met first.")
    w("")
    cats = collections.Counter(category(r.split("|", 1)[1]) for h, r in cov.items() if h not in compiles)
    for c, n in cats.most_common():
        w(f"    {n:5d}  {c}")
    w("")
    w("The most common individual blockers:")
    w("")
    det = collections.Counter(norm(r.split("|", 1)[1])[:110] for h, r in cov.items() if h not in compiles)
    for c, n in det.most_common(40):
        w(f"    {n:5d}  {c}")
    w("")
    hangs = [names.get(h, ["?"])[0] for h, r in cov.items() if r.startswith("False|timeout")]
    if hangs:
        w(f"## Fns the compiler does not finish in {budget}s")
        w("")
        for n in sorted(hangs):
            w(f"    {n}")
        w("")
    if eq:
        w("## Equivalence")
        w("")
        vc = collections.Counter(equiv_verdict(r) for r in eq.values())
        for v, n in vc.most_common():
            w(f"    {n:5d}  {v}")
        w("")
        for label, title in (("DIFF", "Compiled and interpreted DISAGREE (candidate miscompiles)"),
                             ("crash", "Compiled binary crashed"),
                             ("hang", "Compiled binary hung"),
                             ("ierr", "Interpreter raised on the synthesized arguments"),
                             ("cerr", "Compiled binary errored")):
            xs = sorted((names.get(h, ["?"])[0], r) for h, r in eq.items() if equiv_verdict(r) == label)
            if xs:
                w(f"### {title} ({len(xs)})")
                w("")
                for n, r in xs[:60]:
                    w(f"    {n}")
                    w(f"        {r[:150]}")
                if len(xs) > 60:
                    w(f"    ... and {len(xs) - 60} more in the tsv")
                w("")
        un = collections.Counter(norm(r.split("|", 1)[1])[:80] for r in eq.values() if equiv_verdict(r) in ("unprovable", "noargs", "skip"))
        if un:
            w("### Why the harness could not prove the rest")
            w("")
            for c, n in un.most_common(15):
                w(f"    {n:5d}  {c}")
            w("")
    cases = case_verdicts(names, cov, eq)
    if cases:
        w("## Authored cases")
        w("")
        w("`Darklang.CompilerCases.<path>.<target>_<case>` fns, grouped by target. A case")
        w("counts as working when its compiled and interpreted results match byte for byte.")
        w("")
        buckets = collections.Counter(v["bucket"] for v in cases.values())
        for b in ("all cases work", "some cases work", "no case works", "does not compile", "compiles, not swept (--no-equiv)"):
            w(f"    {buckets.get(b, 0):5d}  {b}")
        w("")
        for target, v in sorted(cases.items()):
            w(f"    {v['ok']:2d} / {v['n']:2d}  {target}  ({v['bucket']})")
            for cname, verdict in v["cases"]:
                if verdict != "match":
                    w(f"             {cname}: {verdict[:110]}")
        w("")
    if previous is not None and previous[2] is not None:
        gained, lost = previous[2]
        w("## Since the previous report")
        w("")
        w(f"    newly compiling   {len(gained)}")
        w(f"    no longer compile {len(lost)}")
        if lost:
            w("")
            w("Lost:")
            w("")
            for n, r in lost[:40]:
                w(f"    {n}")
                w(f"        {r[:140]}")
        w("")
    with open(out_md, "w") as f:
        f.write("\n".join(lines) + "\n")
    with open(out_tsv, "w") as f:
        f.write("name\thash\tcompiles\tblocker_category\tblocker\tequiv\tequiv_detail\n")
        for h, r in cov.items():
            ok = r.startswith("True|")
            detail = r.split("|", 1)[1]
            e = eq.get(h, "")
            for n in names.get(h, ["?"]):
                f.write("\t".join([n, h, "yes" if ok else "no", "" if ok else category(detail), detail.replace("\t", " "),
                                   equiv_verdict(e) if e else "", e.replace("\t", " ")]) + "\n")


CASE_PREFIX = "Darklang.CompilerCases."


def case_verdicts(names, cov, eq):
    """target name -> {n, ok, bucket, cases: [(case, verdict)]}."""
    out = collections.defaultdict(lambda: {"n": 0, "ok": 0, "cases": []})
    for h, r in cov.items():
        for n in names.get(h, []):
            if not n.startswith(CASE_PREFIX):
                continue
            path, _, fn = n[len(CASE_PREFIX):].rpartition(".")
            target_fn, _, case = fn.partition("_")
            target = f"Darklang.{path}.{target_fn}"
            if not r.startswith("True|"):
                verdict = "no-compile|" + r.split("|", 1)[1]
            else:
                verdict = eq.get(h, "unswept")
            v = out[target]
            v["n"] += 1
            v["ok"] += verdict.strip() == "match"
            v["cases"].append((case or fn, verdict))
    for v in out.values():
        if all(c[1].startswith("no-compile") for c in v["cases"]):
            v["bucket"] = "does not compile"
        elif all(c[1] == "unswept" or c[1].startswith("no-compile") for c in v["cases"]):
            v["bucket"] = "compiles, not swept (--no-equiv)"
        elif v["ok"] == v["n"]:
            v["bucket"] = "all cases work"
        elif v["ok"] > 0:
            v["bucket"] = "some cases work"
        else:
            v["bucket"] = "no case works"
    return out


def blocker_of(detail):
    """(kind, what) for a coverage failure detail."""
    m = re.match(r"unsupported-builtin: ([A-Za-z0-9]+)(?:_v\d+)?(: .*)?", detail)
    if m:
        kind = "builtin (seam can't return its type)" if "unmarshalable-return" in (m.group(2) or "") else "builtin not routed"
        return kind, "Builtin." + m.group(1)
    m = re.match(r"unsupported-type: (.*)", detail)
    if m:
        return "type", m.group(1)[:120]
    m = re.match(r"unsupported-(value|pattern|generics|call|fnref|expr|literal|infix|pipe|arg): (.*)", detail)
    if m:
        return "bridge: " + m.group(1), m.group(2)[:120]
    if detail.startswith("timeout"):
        return "compiler: timeout", detail[:120]
    if detail.startswith("ANF conversion error"):
        return "compiler: ANF", detail[len("ANF conversion error: "):][:120]
    return "compiler", detail[:120]


def write_functions_csv(out_csv, names, cov, eq, deps):
    import csv
    hashes = list(cov)
    compiles = {h for h, r in cov.items() if r.startswith("True|")}
    first_name = lambda h: names.get(h, ["?"])[0]

    # Depth: callees before callers. Cycles are cut by iterating to a fixpoint with a cap.
    depth = {h: 0 for h in hashes}
    for _ in range(60):
        changed = False
        for h in hashes:
            d = max((depth.get(x, 0) + 1 for x in deps.get(h, ()) if x in depth), default=0)
            if d > depth[h] and d < 60:
                depth[h] = d
                changed = True
        if not changed:
            break

    # A non-compiling fn is "blocked by" the deepest failing callee that has no failing
    # callee of its own (the root cause), if it has one.
    root_cache = {}
    def root_blocker(h, seen=()):
        if h in root_cache:
            return root_cache[h]
        if h in seen:
            return None
        failing = [x for x in deps.get(h, ()) if x in cov and x not in compiles]
        for x in sorted(failing, key=lambda x: depth.get(x, 0)):
            r = root_blocker(x, seen + (h,))
            if r is not None:
                root_cache[h] = r
                return r
            root_cache[h] = x
            return x
        root_cache[h] = None
        return None

    def section(h):
        r = cov[h]
        if h in compiles:
            v = equiv_verdict(eq.get(h, "unswept"))
            if v == "match":
                return "1 proven equal"
            if v in ("DIFF", "crash", "hang", "cerr"):
                return "2 compiles, differs or crashes"
            return "3 compiles, not provable with synthesized args"
        detail = r.split("|", 1)[1]
        if root_blocker(h) is not None:
            return "4 blocked by a callee"
        kind, _ = blocker_of(detail)
        if kind.startswith("builtin"):
            return "5 blocked by a builtin"
        if kind == "type" or kind.startswith("bridge"):
            return "6 blocked by a type or the bridge"
        return "7 blocked by the compiler"

    rows = []
    for h in hashes:
        r = cov[h]
        ok = h in compiles
        detail = r.split("|", 1)[1]
        e = eq.get(h, "")
        meta = EQUIV_META.get(h, {})
        rb = root_blocker(h) if not ok else None
        if ok:
            kind, what = "", ""
        elif rb is not None:
            kind, what = "callee", first_name(rb)
        else:
            kind, what = blocker_of(detail)
        interp = meta.get("interp_ms", "")
        comp = meta.get("compiled_ms", "")
        try:
            ratio = f"{float(comp) / max(float(interp), 0.001):.1f}" if interp and comp else ""
        except ValueError:
            ratio = ""
        for n in names.get(h, ["?"]):
            rows.append({
                "section": section(h),
                "depth": depth[h],
                "name": n,
                "namespace": top_ns(n),
                "compiles": "yes" if ok else "no",
                "verdict": equiv_verdict(e) if (ok and e) else ("" if ok else "no-compile"),
                "blocker_kind": kind,
                "blocker": what,
                "blocker_detail": (detail if not ok else e.split("|", 1)[1] if "|" in e else "").replace("\n", " ")[:300],
                "test_args": meta.get("args", "").replace("\n", " ")[:300],
                "interp_ms": interp,
                "compiled_ms": comp,
                "compiled_over_interp": ratio,
                "direct_callees": len(deps.get(h, ())),
                "hash": h,
            })
    rows.sort(key=lambda x: (x["section"], x["depth"], x["name"]))
    with open(out_csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()) if rows else ["name"])
        w.writeheader()
        for row in rows:
            w.writerow(row)
    return collections.Counter(r["section"] for r in rows)


def read_previous(out_dir, names, cov):
    """(compiled, total, (gained, lost)) from the newest earlier tsv, or None."""
    files = sorted(glob.glob(os.path.join(out_dir, "*.tsv")))
    if not files:
        return None
    prev = {}
    for line in open(files[-1]):
        parts = line.rstrip("\n").split("\t")
        if len(parts) < 5 or parts[0] == "name":
            continue
        prev[parts[1]] = (parts[0], parts[2] == "yes", parts[4])
    pc = sum(1 for v in prev.values() if v[1])
    gained = [names.get(h, ["?"])[0] for h, r in cov.items() if r.startswith("True|") and h in prev and not prev[h][1]]
    lost = [(names.get(h, ["?"])[0], r) for h, r in cov.items() if not r.startswith("True|") and h in prev and prev[h][1]]
    return pc, len(prev), (gained, sorted(lost))


# ---------------------------------------------------------------------------


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--db", default=os.path.join(ROOT, "rundir/data.db"))
    ap.add_argument("--out-dir", default=os.path.join(ROOT, "docs/compiler/coverage"))
    ap.add_argument("--chunk", type=int, default=100, help="fns per compile-sweep process")
    ap.add_argument("--budget", type=int, default=120, help="seconds per compile-sweep chunk before bisecting")
    ap.add_argument("--parallel", type=int, default=4, help="compile-sweep processes at once")
    ap.add_argument("--equiv-chunk", type=int, default=10)
    ap.add_argument("--equiv-budget", type=int, default=60)
    ap.add_argument("--no-equiv", action="store_true", help="skip the equivalence sweep")
    ap.add_argument("--only", help="a name prefix, e.g. Darklang.Stdlib.List, to sweep just that")
    ap.add_argument("--date", default=datetime.date.today().isoformat())
    args = ap.parse_args()

    if not os.path.exists(EXE):
        die(f"no CLI binary at {EXE}; build it flag-on first (see scripts/compiler/report --help)")
    probe = subprocess.run([EXE, "eval", "Builtin.compilerInfo ()"], capture_output=True, text=True)
    if "native compiler linked" not in probe.stdout + probe.stderr:
        die("the CLI binary was not built with -p:DarkWithCompiler=true (compilerInfo is not there)")

    names = package_fns(args.db)
    hashes = sorted(names)
    if args.only:
        hashes = sorted(h for h in hashes if any(n.startswith(args.only) for n in names[h]))
    log = lambda s: print(s, flush=True)
    log(f"coverage sweep: {len(hashes)} fns, chunks of {args.chunk}, {args.budget}s budget, {args.parallel} at a time")
    cov = sweep("compilerCoverageSweep", hashes, args.chunk, args.budget, args.parallel, parse_coverage, log)
    compiling = sorted(h for h, r in cov.items() if r.startswith("True|"))
    eq = {}
    if not args.no_equiv:
        log(f"equivalence sweep: {len(compiling)} fns, chunks of {args.equiv_chunk}, {args.equiv_budget}s budget, {args.parallel} at a time")
        eq = sweep("compilerEquivSweep", compiling, args.equiv_chunk, args.equiv_budget, args.parallel, parse_equiv, log)

    os.makedirs(args.out_dir, exist_ok=True)
    previous = read_previous(args.out_dir, names, cov)
    out_md = os.path.join(args.out_dir, f"{args.date}.md")
    out_tsv = os.path.join(args.out_dir, f"{args.date}.tsv")
    write_report(out_md, out_tsv, names, cov, eq, previous, args.date, args.budget)
    out_csv = os.path.join(args.out_dir, f"{args.date}-functions.csv")
    sections = write_functions_csv(out_csv, names, cov, eq, fn_dependencies(args.db))
    log(f"wrote {os.path.relpath(out_md, ROOT)}, .tsv and -functions.csv")
    for k, n in sorted(sections.items()):
        log(f"  {n:5d}  {k}")


if __name__ == "__main__":
    main()
