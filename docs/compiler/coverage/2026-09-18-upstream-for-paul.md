# Compiler vs interpreter, 2026-09-18, against pbiggar/darklang-compiler at its Sep 15 tip plus the `upstream-dark-fixes` branch

Every package fn was pretty-printed with its closure, compiled from source by the
compiler with its own stdlib in its TestExpression mode, and, where arguments could be
synthesized, run both ways and the JSON diffed. Every fn is in
`2026-09-18-upstream-functions.csv`. The compiler ran with the seventeen commits on
`upstream-dark-fixes` (the PR page lists them); an eighteenth, for the one real
difference below, landed after this run. Yesterday's page (`2026-09-17-upstream-for-paul.md`)
was the same sweep with the first three commits.

## Numbers

    package fns                              5,174
    compile                                  2,770   54%     (yesterday 2,509, 49%)
      Stdlib fns with a compiler implementation  927 of 1,354   68%
      other fns                              1,843 of 3,820   48%
        of those not blocked by a Stdlib or builtin gap   94%   (yesterday 80%)
    run both ways with synthesized inputs    2,423
      identical JSON                         2,361   97%     (yesterday 1,961)
      differ                                    31   (1 real, since fixed; 1 deliberate; 9 float text; 20 host or nondeterministic)
      compiled binary crashed                    1   (yesterday 20)
      compiler or binary past the 60s budget    22   (yesterday 37 hung when called)
      failed to compile when called              8   (yesterday 31)
    could not run                              347   (72 no writable inputs, 274 the interpreter refused the synthesized ones)

"Compiles" is the front end, checker and lowering of what the entry reaches; a fn's
own codegen is exercised by the run. The 2,404 that do not compile, by the leaf cause,
counting every fn that hangs off it:

    2,037  a Stdlib name the compiler's stdlib does not have
             LocalStore 343, Cli.Tui.TerminalSession 272, Cli.FileSystem 203, Env 120,
             HttpClient 109, Pretty 99, Cli.UI.Editor 95, Cli.Tui.Text 81, Cli.Unix 67,
             Cli.File 60, Cli.Tui 58, Sqlite 37, then Char, Cli.Daemon, HttpServer, DB
      240  a Builtin.* of ours (pm*, reflection, scm*) that has no native meaning
       72  the checker (a fold's lambda against a Result-typed accumulator 12, a
             `branchId: Uuid` parameter reported as String 7, `Stdlib.List.singleton`
             absent from your stdlib 7, a lambda's return type not inferable 7, a
             tuple pattern in a lambda whose type is a variable 8, smaller ones)
       23  the compiler does not finish in 60s (the WIP.AI JSON builders; the inliner
             bound in the PR page covers most of yesterday's 34, these are the rest)
       20  lowering (list type not recoverable from an intrinsic 6, argument count
             mismatch when inlining 4, constructor tag collision 7, match in atom
             position 3)
       12  the front end (`Dict<k, v>` with a non-String key, a few let shapes)

## The 31 that differ

- Real (1): `Cli.Http.Serve.parseArgs`. A `when` guard on a list-cons arm that is
  not the last arm was never lowered, so `flag :: _ when startsWith flag "--"` matched
  every non-empty list. The commit "Test a list arm's guard before taking it" fixes it
  and is on the branch; this run predates it.
- Deliberate (1): `CompilerCases.Stdlib.Int64.shiftLeft_byWidth`, `1y << 8y` (masked
  shift count interpreted, 0 compiled).
- Float text (9): `Float.power`, `Float.sqrt`, `Math.atan/degrees/sin/sinh/tan/tanh/turns`.
  The compiler prints the shortest round-trip decimal, the interpreter 15 digits; the
  values agree.
- Host behaviour, not codegen (20): `Cli.Process.pipe/runIn/runWithEnv/shellPipeline/
  spawn` and `Cli.Tests.Instance.*` (the compiler's Cli returns Error where ours
  returns Ok with an exit code), `Cli.Path.resolve/resolveToAbsolute/relativeTo`
  (getcwd unavailable), and the nondeterministic `DateTime.now`, `Sys.currentPid`,
  `String.random`, `Uuid.generate`, `gid` (twice), `gtlid`, `LetPattern.toPT`,
  `MatchPattern.toPT`.

## The one that crashes, and the 22 past the budget

`Cli.Docs.TerminalUI.content` exits 1 (not a SIGSEGV; not looked at). Yesterday's 20
SIGSEGVs were the nested-pattern payload test and a tail call through a closure read
out of a one-field record; both are on the branch.

Of the 22 past 60s, three read stdin (`Cli.UI.Prompt.ask/confirm/select`, the harness's
fault), and the rest are closures over the whole `ProgramTypes.Expr` type: the
pretty-printer, the tokenizer, `Expr.toID`, `WrittenTypes.Expr.range`, the WIP.AI
response parsers. Not measured further in this mode. In FullProgram mode one such
closure (`SCM.Draft.addedHashes`, 23 KB of source, 2.4 MB binary) took 383s, 381s of
it in code generation; the PR page has that.

## Authored cases

109 `Darklang.CompilerCases.*` fns: 106 match, 1 differs (the deliberate shift), 1 the
interpreter refused (`validateLikeOptArg`, the synthesized input raises), 1 does not compile (`List.fold_empty`: a fold over `[]` with an untyped element, the lambda's `acc + x` has no type to infer).
