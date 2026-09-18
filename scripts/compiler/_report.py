#!/usr/bin/env python3.12
"""The compiler coverage report: which package fns compile, which don't and why, and
of those that compile, which provably agree with the interpreter.

Runs in the container (scripts/compiler/report is the entry point). Two sweeps, both
through the flag-on CLI binary and the Dark driver `Darklang.Compiler.Sweep`
(coverage: pretty-print the fn's closure and hand it to the compiler; equivalence:
also run the binary on synthesized arguments and compare with the interpreter), in
chunks, in parallel, with a time budget per chunk. A chunk that overruns is split in two and both halves re-queued, down to one
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
import time
from concurrent.futures import ThreadPoolExecutor, wait, FIRST_COMPLETED

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
EXE = os.path.join(ROOT, "backend/Build/out/Cli/Debug/net10.0/Cli")
HASH = re.compile(r"^[0-9a-f]{64}$")
MEMORY_CAP_GB = 4


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


STDLIB_DIR = os.path.join(ROOT, "backend/src/LibCompiler/stdlib")


def compiler_stdlib_names():
    """Every `Module.name` the compiler's own stdlib declares (fns, values, types),
    from its .dark files. A Stdlib fn of ours is "covered" when the compiler has
    one by that name; its body is not compiled, the compiler's is."""
    names = set()
    for path in glob.glob(os.path.join(STDLIB_DIR, "*.dark")):
        module = None
        for line in open(path, encoding="utf-8"):
            m = re.match(r"module ([A-Za-z0-9_.]+)\s*$", line)
            if m:
                module = m.group(1)
                continue
            m = re.match(r"(?:let|val|type) ([A-Za-z_][A-Za-z0-9_]*)", line)
            if m and module:
                names.add(f"{module}.{m.group(1)}")
    # The intrinsics (Stdlib.Bool.not, the bitwise fns, the Cli primitives...) are
    # declared in F#, as ModuleDefs with a Name and a list of { Name = ... } fns.
    module = None
    for line in open(os.path.join(STDLIB_DIR, "..", "Stdlib.fs"), encoding="utf-8"):
        m = re.search(r'Name = \$?"(Stdlib\.[A-Za-z0-9_.{}]+)"', line)
        if m:
            module = m.group(1)
            if "{" in module:  # a format string: `Stdlib.{name}` for the sized-int modules
                module = None
            continue
        m = re.search(r'\{ Name = "([A-Za-z_][A-Za-z0-9_]*)"', line)
        if m and module:
            names.add(f"{module}.{m.group(1)}")
    for typ in ("Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Int128", "UInt128"):
        for fn in ("bitwiseAnd", "bitwiseOr", "bitwiseXor", "shiftLeft", "shiftRight", "bitwiseNot"):
            names.add(f"Stdlib.{typ}.{fn}")
    return names


def is_stdlib(name):
    return name.startswith("Darklang.Stdlib.")


def compiler_name(name):
    return name[len("Darklang."):] if is_stdlib(name) else name


# ---------------------------------------------------------------------------
# Running a sweep builtin over a chunk, with bisection
# ---------------------------------------------------------------------------


class Worker:
    """One long-lived flag-on CLI process running `Darklang.Compiler.Sweep.serve ()`,
    fed one request per line on stdin. The process start and the compiler's stdlib
    build cost more than most compiles, so a worker lives for the whole sweep and
    is only restarted after a timeout, a death, or the memory cap."""

    def __init__(self):
        self.proc = None
        self.lines = None
        self.scratch = None

    def start(self):
        import queue, threading
        self.stop()
        # cwd is a throwaway dir: the equivalence sweep RUNS fns in the interpreter
        # with synthesized arguments, and one of them wrote a 51 MB store copy named
        # "hello" into the repo root before this was here.
        self.scratch = tempfile.mkdtemp(prefix="dark-sweep-")
        env = {**os.environ,
               "DARK_CONFIG_RUNDIR": os.environ.get("DARK_CONFIG_RUNDIR", os.path.join(ROOT, "rundir"))}

        # A runaway compile once grew to 39 GB and took the machine down with it;
        # cap each worker's address space so it dies alone instead.
        def cap():
            import resource
            limit = MEMORY_CAP_GB * 1024 ** 3
            resource.setrlimit(resource.RLIMIT_AS, (limit, limit))

        self.proc = subprocess.Popen([EXE, "eval", "Darklang.Compiler.Sweep.serve ()"],
                                     stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=subprocess.DEVNULL, text=True, env=env,
                                     cwd=self.scratch, preexec_fn=cap)
        self.lines = queue.Queue()
        proc = self.proc

        def pump():
            for line in proc.stdout:
                self.lines.put(line.rstrip("\n"))
            self.lines.put(None)

        threading.Thread(target=pump, daemon=True).start()

    def stop(self):
        if self.proc is not None:
            try:
                self.proc.kill()
                self.proc.wait(timeout=10)
            except Exception:
                pass
            self.proc = None
        if self.scratch:
            shutil.rmtree(self.scratch, ignore_errors=True)
            self.scratch = None

    def request(self, line, timeout):
        """(rc, output) where rc is 0 for a complete reply, 124 for a timeout (the
        worker is restarted) and the exit code when the process died mid-reply."""
        import queue
        if self.proc is None or self.proc.poll() is not None:
            self.start()
        try:
            self.proc.stdin.write(line + "\n")
            self.proc.stdin.flush()
        except (BrokenPipeError, OSError):
            rc = self.proc.poll() or 1
            self.start()
            return rc, ""
        out = []
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                self.start()
                return 124, "\n".join(out)
            try:
                got = self.lines.get(timeout=min(remaining, 5))
            except queue.Empty:
                continue
            if got is None:
                rc = self.proc.wait()
                self.start()
                return (rc or 1), "\n".join(out)
            if got == "##done##":
                return 0, "\n".join(out)
            out.append(got)


class WorkerPool:
    """`parallel` threads, each owning one Worker. `run(fn, arg)` calls
    fn(worker, arg) on a free thread and returns a future."""

    def __init__(self, parallel):
        import threading
        self.pool = ThreadPoolExecutor(max_workers=parallel)
        self.local = threading.local()
        self.workers = []
        self.lock = threading.Lock()

    def _worker(self):
        w = getattr(self.local, "w", None)
        if w is None:
            w = Worker()
            self.local.w = w
            with self.lock:
                self.workers.append(w)
        return w

    def run(self, fn, arg):
        return self.pool.submit(lambda: fn(self._worker(), arg))

    def close(self):
        self.pool.shutdown(wait=True)
        for w in self.workers:
            w.stop()


PROGRESS = os.path.join(ROOT, "rundir/logs/compiler-sweep.log")
_progress_lock = __import__("threading").Lock()


def progress(line):
    """One line per finished request, to rundir/logs/compiler-sweep.log: what is
    slow, what died, how far along it is. Stdout is buffered by the container
    wrapper and says nothing until the end."""
    with _progress_lock:
        with open(PROGRESS, "a") as f:
            f.write(time.strftime("%H:%M:%S ") + line + "\n")


def dark_list(hashes):
    return "[" + ",".join(f'"{h}"' for h in hashes) + "]"


# ---------------------------------------------------------------------------
# The dependency graph, and what is known before any compile
# ---------------------------------------------------------------------------


def package_items(db):
    """hash -> (kind, [names]) for every listed item of every kind."""
    con = sqlite3.connect(db)
    rows = con.execute(
        "SELECT item_hash, item_type, owner || '.' || modules || '.' || name FROM locations "
        "WHERE unlisted_at IS NULL ORDER BY 3"
    ).fetchall()
    con.close()
    items = {}
    for h, kind, name in rows:
        items.setdefault(h, (kind, []))[1].append(name)
    return items


def item_dependencies(db):
    """hash -> set of hashes it depends on directly, every item kind."""
    con = sqlite3.connect(db)
    rows = con.execute("SELECT item_hash, depends_on_hash FROM package_dependencies").fetchall()
    con.close()
    deps = collections.defaultdict(set)
    for a, b in rows:
        if a != b:
            deps[a].add(b)
    return deps


def closures(hashes, deps, items):
    """hash -> the set of items its compile needs: everything reachable, stopping at
    Stdlib items (the compiler has its own), which are included as leaves."""
    memo = {}

    def walk(h):
        if h in memo:
            return memo[h]
        memo[h] = set()  # cycle guard
        out = {h}
        kind_names = items.get(h)
        if kind_names and not is_stdlib(kind_names[1][0]):
            for d in deps.get(h, ()):
                out |= walk(d)
        memo[h] = out
        return out

    return {h: walk(h) for h in hashes}


def depths(hashes, deps, items):
    """Callees before callers: the longest path to a leaf over non-Stdlib fn edges,
    with cycles cut at a cap. Fns at the same depth are independent."""
    fn_set = set(hashes)
    depth = {h: 0 for h in hashes}
    for _ in range(80):
        changed = False
        for h in hashes:
            d = max((depth[x] + 1 for x in deps.get(h, ()) if x in fn_set and x != h), default=0)
            if d > depth[h] and d < 80:
                depth[h] = d
                changed = True
        if not changed:
            break
    return depth


# ---------------------------------------------------------------------------
# Coverage: one compile per group of fns whose callees already compile
# ---------------------------------------------------------------------------


GROUP_REC = re.compile(r"^(\d+)\t(.*)$")


def coverage_sweep(pool, hashes, items, deps, failed, parallel, budget, log):
    """cov: hash -> 'True|..' / 'False|..' for every fn in `hashes`, without
    compiling most of them.

    `failed` comes in seeded with the Stdlib items the compiler lacks (by hash) and
    leaves with every item found not to compile, with its message.

    Fns go in dependency order. A fn whose closure already holds a failed item is
    blocked, no compile. The rest, grouped by module, get ONE compile of the merged
    closure per group; a group that fails is halved until the failing fns are
    known. Several groups share a process, since the process start and the
    compiler's stdlib build are the fixed cost."""
    cov = {}
    closure = closures(hashes, deps, items)
    depth = depths(hashes, deps, items)
    by_depth = collections.defaultdict(list)
    for h in hashes:
        by_depth[depth[h]].append(h)
    module_of = lambda h: items[h][1][0].rsplit(".", 1)[0]

    def blocked_by(h):
        bad = [x for x in closure[h] if x in failed and x != h]
        if not bad:
            return None
        # the deepest culprit is the most useful one to name; ties by name
        return sorted(bad, key=lambda x: (depth.get(x, -1), items.get(x, ("", ["?"]))[1][0]))[0]

    def run_group(worker, group):
        """(group, result, died) for one compile of the merged closure."""
        t0 = time.monotonic()
        rc, out = worker.request("cov " + " ".join(group), budget)
        progress(f"cov {len(group):3d} fns {time.monotonic() - t0:6.1f}s rc={rc} {module_of(group[0])} {out.strip()[:100]}")
        if rc == 124:
            return group, "timeout|the compiler ran past the %ds budget" % budget, True
        if rc != 0:
            return group, "timeout|the worker died (rc %d), likely over the %d GB memory cap" % (rc, MEMORY_CAP_GB), True
        return group, " ".join(l.strip() for l in out.split("\n") if l.strip()) or "no-compile|<no record in the sweep output>", False

    compiles = [0]
    for d in sorted(by_depth):
        level = by_depth[d]
        pending_groups = collections.defaultdict(list)
        for h in level:
            b = blocked_by(h)
            if b is not None:
                bname = items.get(b, ("", ["?"]))[1][0]
                cov[h] = f"False|blocked by callee {bname}: {failed[b]}"
            else:
                pending_groups[module_of(h)].append(h)
        queue = sorted(pending_groups.values(), key=len, reverse=True)
        log(f"depth {d}: {len(level)} fns, {sum(1 for h in level if h in cov)} blocked, {len(queue)} groups to compile")
        # No barrier inside a depth: a worker that frees up takes whatever group is
        # queued, including the halves of a group that just failed.
        running = set()
        while queue or running:
            while queue and len(running) < parallel:
                running.add(pool.run(run_group, queue.pop(0)))
            done, running = wait(running, return_when=FIRST_COMPLETED)
            for fut in done:
                g, r, died = fut.result()
                compiles[0] += 1
                if r == "ok":
                    for h in g:
                        cov[h] = "True|compiled with its module"
                elif len(g) == 1:
                    cov[g[0]] = "False|" + r.split("|", 1)[1] if "|" in r else "False|" + r
                    failed[g[0]] = r.split("|", 1)[1] if "|" in r else r
                else:
                    half = (len(g) + 1) // 2
                    queue.append(g[:half])
                    queue.append(g[half:])
    log(f"coverage: {compiles[0]} group compiles for {len(hashes)} fns")
    return cov


# ---------------------------------------------------------------------------
# Equivalence: one binary per batch, resumed past a crash
# ---------------------------------------------------------------------------


REC = re.compile(r"^([0-9a-f]{64})\t(.*)$")
MARK = "##dark-sweep##"
EQUIV_META = {}  # hash -> {"args": the call}


def equivalence_sweep(pool, hashes, items, parallel, budget, batch_size, run_timeout, log):
    """eq: hash -> verdict (match | DIFF|c=..|i=.. | noargs | ierr | crash | timeout
    | no-compile) for every compiling fn.

    Fns are batched by module. One process evaluates every fn of the batch in the
    interpreter and builds ONE binary that prints every compiled result on its own
    marked line. A crash or timeout ends the binary at some fn: the lines before it
    are results, the first fn without a line is charged, and the rest are re-queued.
    A batch that does not build is halved until the fn that does not build when
    called is known."""
    eq = {}
    module_of = lambda h: items[h][1][0].rsplit(".", 1)[0]
    by_module = collections.defaultdict(list)
    for h in hashes:
        by_module[module_of(h)].append(h)
    queue = []
    for hs in by_module.values():
        queue.extend(hs[i:i + batch_size] for i in range(0, len(hs), batch_size))

    def run_batch(worker, batch):
        t0 = time.monotonic()
        rc, out = worker.request(f"eq {run_timeout * 1000} " + " ".join(batch), budget)
        progress(f"eq  {len(batch):3d} fns {time.monotonic() - t0:6.1f}s rc={rc} {module_of(batch[0])}")
        return batch, rc, out

    binaries = [0]
    log(f"equivalence: {len(queue)} batches, {len(hashes)} fns")
    running = set()
    if True:
        while queue or running:
            while queue and len(running) < parallel:
                running.add(pool.run(run_batch, queue.pop(0)))
            done, running = wait(running, return_when=FIRST_COMPLETED)
            for fut in done:
                batch, rc, out = fut.result()
                if rc != 0:
                    # a timeout, or the worker died (an interpreter-side crash takes
                    # the whole process with it): halve, and name it on a single
                    if len(batch) == 1:
                        eq[batch[0]] = ("timeout|the whole process ran past the %ds budget" % budget) if rc == 124 \
                            else "crash|the worker process died (rc %d), interpreter side or compiler" % rc
                    else:
                        half = (len(batch) + 1) // 2
                        queue.append(batch[:half])
                        queue.append(batch[half:])
                    continue
                binaries[0] += 1
                interp = {}
                status, detail = "missing", ""
                printed = {}
                cur = None
                for line in out.split("\n"):
                    if line.startswith(MARK):
                        h, _, val = line[len(MARK):].partition("\t")
                        printed[h] = val.strip()
                        cur = None
                        continue
                    m = REC.match(line)
                    if m:
                        cur, val = m.group(1), m.group(2)
                        val, _, args = val.partition("\targs=")
                        if args:
                            EQUIV_META[cur] = {"args": args}
                        interp[cur] = val
                        continue
                    if line.startswith("batch\t"):
                        status, _, detail = line[len("batch\t"):].partition("|")
                        cur = None
                        continue
                    if cur and line.strip():
                        interp[cur] += " " + line.strip()
                runnable = [h for h in batch if interp.get(h, "").startswith("interp|")]
                for h in batch:
                    v = interp.get(h, "missing|no record in the sweep output")
                    if not v.startswith("interp|"):
                        eq[h] = v
                if status == "compile-error":
                    if len(runnable) == 1:
                        # The entry serializes the result to JSON to compare it; a
                        # result with no JSON form (a function, a Blob) is the
                        # harness's limit, not a failure of the fn to compile.
                        if "Unsupported type in JSON" in detail:
                            eq[runnable[0]] = "noargs|no JSON form for the result: " + detail
                        else:
                            eq[runnable[0]] = "no-compile|" + detail
                    elif runnable:
                        half = (len(runnable) + 1) // 2
                        queue.append(runnable[:half])
                        queue.append(runnable[half:])
                    continue
                # results in order; the first fn without a line is where the binary
                # stopped, and everything after it goes back as one batch
                stopped = None
                rest = []
                for h in runnable:
                    if h in printed:
                        expected = interp[h][len("interp|"):].strip()
                        got = printed[h]
                        if got == expected:
                            eq[h] = "match"
                        else:
                            # a window around the first differing character, so a
                            # long equal prefix does not hide the difference
                            k = next((j for j in range(min(len(got), len(expected))) if got[j] != expected[j]),
                                     min(len(got), len(expected)))
                            lo = max(0, k - 120)
                            eq[h] = f"DIFF at {k} of {len(got)}/{len(expected)}|c={got[lo:k + 200]}|i={expected[lo:k + 200]}"
                    elif stopped is None:
                        stopped = h
                        eq[h] = "missing|the binary printed nothing for it" if status == "ran" else f"{status}|{detail}"
                    else:
                        rest.append(h)
                if rest:
                    queue.append(rest)
    log(f"equivalence: {binaries[0]} binaries for {len(hashes)} fns")
    return eq


# ---------------------------------------------------------------------------
# Classifying
# ---------------------------------------------------------------------------


def norm(d):
    d = re.sub(r"[0-9a-f]{64}", "<h>", d)
    d = re.sub(r"`[^`]*`", "`_`", d)
    d = re.sub(r"\d+ bytes", "<n> bytes", d)
    return d


def blocker_of(detail):
    """(kind, what) for a coverage failure message from the compiler.

    The kinds are the buckets the report and the CSV sections use:
      stdlib gap        our Stdlib has it, the compiler's does not (a name or a type)
      builtin           a `Builtin.x` of ours the compiler has no implementation of
      parse gap         the compiler's front end rejects source the interpreter accepts
      type gap          Dict<k, v> with a non-String key, and other type-level differences
      compiler          the compiler rejects or fails on a program it parsed and resolved
      timeout           the compile did not finish in the budget
    """
    m = re.match(r"blocked by callee (\S+): (.*)", detail)
    if m:
        return "callee", m.group(1)
    m = re.match(r"There is no variable named: Builtin\.([A-Za-z0-9_]+)", detail)
    if m:
        return "builtin", "Builtin." + m.group(1)
    m = re.match(r"(?:There is no variable named|Unknown type reference|Unresolved (?:type|value|constructor) name): (Stdlib\.[A-Za-z0-9_.]+)", detail)
    if m:
        return "stdlib gap", m.group(1)
    m = re.match(r"Unknown type reference: ([A-Za-z0-9_.]+) in", detail)
    if m:
        return "stdlib gap" if m.group(1).startswith("Stdlib.") else "compiler", m.group(1)
    if detail.startswith("Parse error: Dict expects exactly one type argument"):
        return "type gap", "Dict with a non-String key"
    if detail.startswith("Parse error"):
        return "parse gap", detail[len("Parse error: "):][:120]
    if detail.startswith("timeout"):
        return "timeout", detail[:120]
    if detail.startswith("Constructor identity collision"):
        return "compiler", "constructor tag collision (12-bit name hash)"
    if detail.startswith("ANF conversion error"):
        return "compiler", "ANF: " + detail[len("ANF conversion error: "):][:100]
    if detail.startswith("stdlib:"):
        return "compiler", "the compiler's own stdlib did not build: " + detail[:100]
    return "compiler", detail[:120]


def category(detail):
    kind, what = blocker_of(detail)
    if kind == "callee":
        return "blocked by a callee"
    if kind in ("builtin", "stdlib gap", "type gap", "parse gap", "timeout"):
        return kind
    if what.startswith("ANF:"):
        return "compiler: lowering"
    if any(k in detail for k in ("expects", "Type mismatch", "Failed to create record", "Unknown record type", "Cannot apply", "is not a function")):
        return "compiler: type check"
    return "compiler: other"


def equiv_verdict(r):
    # A DIFF verdict carries where the outputs part ("DIFF at 12 of 40/38") in
    # front of the bar; it is one verdict for counting.
    v = r.split("|", 1)[0].strip() or "?"
    return "DIFF" if v.startswith("DIFF") else v


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
    w("\"Compiles\" means the fn and its whole closure, pretty-printed as Dark source, went")
    w("through the compiler's front end, checker and lowering without an error. \"Match\"")
    w("means the compiled binary and the interpreter produced the same JSON for the same")
    w("synthesized arguments; \"noargs\" means the harness could not write arguments (a")
    w("custom-typed parameter it cannot build), which says nothing about correctness")
    w("either way. Stdlib fns are the compiler's own implementations, called from the entry.")
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
    w("By root cause: a fn blocked only by something it calls is counted under that")
    w("callee's own error, so these are the leaf problems, weighted by how many fns")
    w("hang off each.")
    w("")

    def root_message(r):
        m = re.match(r"blocked by callee \S+: (.*)", r.split("|", 1)[1], re.S)
        return m.group(1) if m else r.split("|", 1)[1]

    def root_item(h, r):
        m = re.match(r"blocked by callee (\S+): ", r.split("|", 1)[1])
        return m.group(1) if m else names.get(h, ["?"])[0]

    failing = {h: r for h, r in cov.items() if h not in compiles}
    cats = collections.Counter(category(root_message(r)) for r in failing.values())
    for c, n in cats.most_common():
        w(f"    {n:5d}  {c}")
    w("")
    w("The leaf problems, with the number of fns each blocks (itself included):")
    w("")
    roots = collections.Counter(root_item(h, r) for h, r in failing.items())
    root_msg = {}
    for h, r in failing.items():
        root_msg.setdefault(root_item(h, r), root_message(r))
    for item, n in roots.most_common(40):
        w(f"    {n:5d}  {item}")
        w(f"           {norm(root_msg[item])[:140]}")
    w("")
    w("The same, by error message:")
    w("")
    det = collections.Counter(norm(root_message(r))[:110] for r in failing.values())
    for c, n in det.most_common(30):
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
                             ("timeout", "Compiled binary ran past the deadline"),
                             ("no-compile", "Compiled alone, failed when called with synthesized arguments"),
                             ("ierr", "Interpreter raised on the synthesized arguments")):
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
            if v in ("DIFF", "crash", "hang", "cerr", "timeout"):
                return "2 compiles, differs or crashes"
            if v == "no-compile":
                return "3 compiles alone, not when called"
            return "4 compiles, not provable with synthesized args"
        detail = r.split("|", 1)[1]
        if root_blocker(h) is not None or detail.startswith("blocked by callee"):
            return "5 blocked by a callee"
        kind, _ = blocker_of(detail)
        if kind == "builtin":
            return "6 blocked by a builtin the compiler lacks"
        if kind == "stdlib gap":
            return "7 blocked by a Stdlib gap"
        if kind in ("parse gap", "type gap"):
            return "8 blocked by a front-end gap"
        return "9 blocked by the compiler"

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
            if kind == "callee":
                what = what
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
    ap.add_argument("--budget", type=int, default=60, help="seconds one group compile may take (a normal one takes under 2; a hang costs the whole budget at every halving)")
    ap.add_argument("--parallel", type=int, default=6, help="sweep processes at once (each capped at %d GB; keep this small, the machine is shared)" % MEMORY_CAP_GB)
    ap.add_argument("--equiv-batch", type=int, default=25, help="fns per equivalence binary")
    ap.add_argument("--equiv-budget", type=int, default=180, help="seconds one equivalence batch may take, interpreter and binary included")
    ap.add_argument("--run-timeout", type=int, default=20, help="seconds a compiled binary may run")
    ap.add_argument("--no-equiv", action="store_true", help="skip the equivalence sweep")
    ap.add_argument("--only", help="a name prefix, e.g. Darklang.Stdlib.List, to sweep just that")
    ap.add_argument("--date", default=datetime.date.today().isoformat())
    args = ap.parse_args()

    if not os.path.exists(EXE):
        die(f"no CLI binary at {EXE}; build it flag-on first (see scripts/compiler/report --help)")
    probe = subprocess.run([EXE, "eval", "Darklang.Compiler.Sweep.info ()"], capture_output=True, text=True)
    if "native compiler linked" not in probe.stdout + probe.stderr:
        die("the CLI binary was not built with -p:DarkWithCompiler=true (compilerInfo is not there)")

    names = package_fns(args.db)
    items = package_items(args.db)
    deps = item_dependencies(args.db)
    hashes = sorted(names)
    if args.only:
        hashes = sorted(h for h in hashes if any(n.startswith(args.only) for n in names[h]))
    def log(s):
        print(s, flush=True)
        progress(s)
    progress("---- sweep start ----")

    # Stdlib items are not compiled from our source; the compiler has its own. What it
    # lacks (by name) seeds the failed set, so a fn using it is blocked without a compile.
    known = compiler_stdlib_names()
    failed = {}
    for h, (kind, ns) in items.items():
        if is_stdlib(ns[0]) and not any(compiler_name(n) in known for n in ns):
            failed[h] = f"There is no variable named: {compiler_name(ns[0])}" if kind != "type" \
                else f"Unknown type reference: {compiler_name(ns[0])}"
    stdlib_hashes = [h for h in hashes if is_stdlib(names[h][0])]
    other_hashes = [h for h in hashes if not is_stdlib(names[h][0])]
    cov = {}
    for h in stdlib_hashes:
        cov[h] = f"False|{failed[h]}" if h in failed else "True|in the compiler's stdlib"
    log(f"stdlib: {sum(1 for h in stdlib_hashes if cov[h].startswith('True|'))} of {len(stdlib_hashes)} fns have a compiler implementation")
    log(f"coverage sweep: {len(other_hashes)} fns in dependency order, {args.parallel} workers")
    pool = WorkerPool(args.parallel)
    try:
        cov.update(coverage_sweep(pool, other_hashes, items, deps, failed, args.parallel, args.budget, log))
        compiling = sorted(h for h, r in cov.items() if r.startswith("True|"))
        eq = {}
        if not args.no_equiv:
            eq = equivalence_sweep(pool, compiling, items, args.parallel, args.equiv_budget, args.equiv_batch, args.run_timeout, log)
    finally:
        pool.close()

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
