# Handing over the "which package fns compile" effort

## What the effort is for

Dark has one runtime today, the F# interpreter, and your compiler is meant to become the second: the same package tree, compiled to a native binary. The question this effort answers, with a number that can be re-measured on every compiler commit, is how far that is from true for the code we actually have: the ~5,200 functions in darklang/dark's `packages/` (the stdlib, the CLI, the language tools, the SCM), not hand-written test programs. For each one, three questions: does it compile at all, and if not, what is the first thing in the way (a missing stdlib fn, one of our own builtins, a front-end gap, a checker or lowering bug); if it compiles, does the binary produce the same result as the interpreter on the same inputs; and when the two disagree, is it the compiler, the harness, or a real difference in semantics we want to know about.

The value is in the second and third answers. A miscompile that agrees with the interpreter 97% of the time is exactly the kind you never find by writing tests for the shapes you thought of; every SIGSEGV, hang and wrong answer in the PR came from a real package function nobody wrote a test for. And the blocker breakdown turns "the compiler can't run the CLI yet" into a list with counts: this many functions are waiting on LocalStore, this many on Cli.Tui, this many on a lowering bug with a named reproducer. That is what decides what to build next, and it is what the numbers at the top of both PRs are.

The way it is set up is meant to keep that measurement cheap as the compiler moves: nothing of the compiler is patched or forked on our side, fixes go to your repo one commit each with a failing test, the dark side re-vendors your source at a commit and re-runs the sweep, and the report diffs itself against the previous run. So a future "how much of Dark compiles now" is one command, not a project.

## What this folder is

The notes Stachu's side wrote while doing that, kept in the repo so someone else (you, or whoever picks it up) can run the loop without us. The code itself is in two branches: `darklang/compiler:upstream-dark-fixes` (18 fixes to the compiler, one PR) and `StachuDotNet/dark:compiler-merge-rebased` (the sweep tooling on the dark side, one PR). The results live beside this folder in `docs/compiler/coverage/`.

## Read in this order

1. `compiler-PR-to-paul-2026-09-18-short.md`: the compiler PR as posted. One page.
2. `../coverage/2026-09-18-upstream-for-paul.md`: the numbers, what blocks what, the 31 differences, the crash. `../coverage/2026-09-18-upstream-functions.csv` is the per-function version (name, hash, compiles, blocker, equivalence verdict, the synthesized arguments), which is what to grep when you want a reproducer for a given message.
3. `compiler-patch-PR-description-2026-09-18.md`: the long form of the PR: each commit, the decisions worth a skeptical read, and "Not in this PR", the itemized list of what is still the compiler's, with counts and a named fn for each.
4. `PR-compiler-merge-rebased-description-2026-09-18.md` and `compiler-integration-how-it-stands-2026-09-18.md`: how the dark side works and how to re-run it. The "native compiler" section of `AGENTS.md` is the same loop as the repo documents it; `backend/src/LibCompiler/vendor/README.md` is how the compiler source gets copied in.
5. `../coverage/2026-09-18-upstream.md` / `.tsv`: the raw generated report (by namespace, by blocker message, equivalence buckets).

## The loop, if you run it yourself

Clone `StachuDotNet/dark` at `compiler-merge-rebased`, start its dev container (`scripts/dev/start`), then:

    scripts/build/vendor-compiler <your compiler checkout> [commit]
    ./scripts/build/_dotnet-wrapper build --configuration Debug -p:DarkWithCompiler=true src/Cli/Cli.fsproj
    ./scripts/build/reload-packages
    scripts/compiler/report --date <label>            # ~30 min, 6 workers; --only Darklang.Cli.Packages is a minute
    scripts/compiler/upstream-tests                   # your suite, in that container, --e2e-batch-size=64

Output lands in `docs/compiler/coverage/<label>{.md,.tsv,-functions.csv}`, and the `.md` diffs itself against the previous run. `packages/darklang/compiler/sweep.dark` is the part that turns a package fn into source your compiler accepts (name respelling, closure printing, argument synthesis); `scripts/compiler/_report.py` is the driver.

## Reproducers

Any fn in the csv can be turned into its source units with `Darklang.Compiler.Sweep.unitsFor` (see `sweep.dark`); that is how every reproducer in the PR was made. Two that are not in the PR, kept outside the repo in Stachu's handover folder because they are source dumps, not notes:

- `Darklang.SCM.Draft.addedHashes` with its closure (23 KB, 18 units). As library units plus a `()` entry in TestExpression mode it compiles in 7s (tree-shaken to 632 bytes); in FullProgram mode 383s, 381s of it in `[backend.codegen] Code Generation`, for a 2.4 MB binary. The sweep saw 32 closures like it.
- An e2e file whose preamble crashes the harness in `rcShapeOfTypeWithSums` ("Record type 'ZParser2' not found in typeReg"): a fold over a record with a function-typed field, seeded by a call. The same program compiles and runs as a file. (Written out in full under "Not in this PR" on the long PR page.)

## What we did not do, on purpose

No stdlib surface was added to your repo beyond `Result.mapError`'s argument order. 2,037 of the 2,404 non-compiling fns hang off Stdlib modules your stdlib does not have (LocalStore, Cli.Tui, FileSystem, Env, HttpClient, Pretty); which of those you want native is your call, and no compiler fix moves them. Our own `Builtin.*` (240 fns) are ours to deal with.
