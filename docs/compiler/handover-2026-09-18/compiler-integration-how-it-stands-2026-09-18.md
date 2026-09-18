# The compiler integration, morning of 2026-09-18: what is where, and how to keep it alive

Supersedes `compiler-integration-how-it-stands-2026-09-17.md`. The scheme is the same;
this is the state after the overnight run and what I learned running it.

---

## What exists now

- `~/code/compiler`, branch `upstream-dark-fixes`: eighteen commits on Paul's Sep 15 tip,
  one thing each, every one with a test that fails on main.
  Not pushed. The PR description is `compiler-patch-PR-description-2026-09-18.md`.
- `~/code/dark/compiler-merge-2`, branch `compiler-merge-rebased`: the dark side. The
  compiler copy under `backend/src/LibCompiler/` is untracked; only `vendor/` (what was
  copied, from where) is in git. `Builtins.Compiler` is six thin builtins; everything
  about which source to hand the compiler lives in `packages/darklang/compiler/sweep.dark`.
  `scripts/compiler/report` is the sweep; `scripts/compiler/upstream-tests` runs Paul's
  suite in the container from a copy of the checkout. Nothing pushed. Clone 1
  (`~/code/dark/compiler-merge`) is fast-forwarded to the same commit.
- The results: `docs/compiler/coverage/2026-09-18-upstream{.md,.tsv,-functions.csv}` and
  the one-pager `2026-09-18-upstream-for-paul.md` (copies beside this file as
  `compiler-report-for-paul-2026-09-18-upstream.md` and
  `compiler-report-functions-2026-09-18-upstream.csv`). Earlier runs are kept under
  `rundir/coverage-run5..7/` for diffing, not committed.

---

## The loop, from here

1. A compiler fix is made in `~/code/compiler` on a branch, with an e2e case, checked
   with `scripts/compiler/upstream-tests` (rsync the checkout into
   `backend/src/LibCompiler/upstream/` first; rsync keeps mtimes, so `touch` the file
   you changed or the build keeps a stale DLL, which cost me one wrong suite result
   tonight). To show a case fails on main, drop it into `upstream-pristine/` and run
   the same command there. It goes to Paul as a PR. Nothing lives in the dark tree.
2. `scripts/build/vendor-compiler ~/code/compiler`, build flag-on, reload packages, and
   `scripts/compiler/report`. About 30 minutes for the tree, 6 workers, each capped at
   4 GB. `--only Darklang.Cli.Packages` is a minute.
3. When Paul merges, `vendor-compiler ~/code/compiler paul/main` and the same report
   against the new tip; the report diffs against the previous run.

The sweep compiles in the compiler's "eval" mode (TestExpression, what his e2e harness
uses), not "program" mode (FullProgram, what `dark file.dark` uses). I tried program
mode for one run: 32 closures that eval mode compiles in seconds ran past the budget.
One measured: `SCM.Draft.addedHashes`, 23 KB of source, 7s in eval mode (tree-shaken to
632 bytes) and 383s in program mode, 381s of it in `[backend.codegen] Code Generation`
for a 2.4 MB binary. That is on the PR page for Paul. Eval mode had its own gap, a
stdlib specialization a library unit's generic reaches that nobody requested, which is
one of the later commits. The sweep stays on eval.

---

## What moved overnight

    yesterday evening (3 commits)   this morning (17 of the 18)
    compile        2,509  49%        2,770  54%
    identical      1,961             2,361
    differ            32                31   (1 real, fixed as the 18th commit; the rest float text, host, random)
    crash             20                 1
    hang/past 60s     37                22
    fail when called  31                 8

Of the 2,404 that do not compile, 2,037 are a Stdlib name Paul's stdlib lacks and 240
a `Builtin.*` of ours; 127 are the compiler's. Full breakdown on the one-pager.

Three classes closed: the SIGSEGVs (nested-pattern payloads, list heads, and a tail
call through a closure read out of a one-field record), the hangs (the inliner copying
continuations, 76 million ANF nodes for a fourteen-field record equality), and the
Stachu.Parser combinators (type parameters freshened by index, so every generic call in
a body shared `a$0`). What is left is mostly not compiler work: the stdlib surface, our
own `Builtin.*`, and the constructor tag hash.

---

## What I got wrong, so it is written down

- Yesterday: 8 workers, a 39 GB worker, and Paul's suite at the same time froze the
  box. Workers are capped and sequential now.
- A test file passed when run alone and crashed in the full suite, because the rsync
  had not bumped the mtime of the file I had reverted and restored; the DLL was stale.
  `touch` after rsync, always.
- My first freshening change treated any type variable named `t` as bindable, which
  let a declared `'t` unify with `'u` and moved one of his error tests. The empty-list
  variable is now spelled `t$empty` so it cannot be mistaken for a declared one.
- I ran one sweep in program mode expecting only the 7 missing specializations to
  move, and got 32 new hangs and 40 new failures instead (the failures turned out to be
  a real checker bug the freshening had uncovered, fixed since; the hangs are Paul's).
