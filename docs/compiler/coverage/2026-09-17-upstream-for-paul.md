# Compiler vs interpreter, 2026-09-17, against pbiggar/darklang-compiler at its Sep 15 tip

Every package fn was pretty-printed with its closure, compiled from source by the
compiler with its own stdlib, and, where arguments could be synthesized, run both
ways and the JSON diffed. Every fn is in `2026-09-17-upstream-functions.csv`. The
compiler ran with the three front-end commits on `upstream-dark-fixes` (let bodies
and statement sequences from indentation, bare self-names, Int literals against type
variables); without them a two-let function misparses and almost nothing compiles.

## Numbers

    package fns                              5,171
    compile                                  2,509   49%
      Stdlib fns with a compiler implementation  927 of 1,354   68%
      other fns                              1,582 of 3,817   41%
        of those not blocked by a Stdlib or builtin gap   80%
    run both ways with synthesized inputs    2,084
      identical JSON                         1,961   94%
      differ                                    32
      compiled binary crashed                   20
      compiler hung when the fn was called      37
      ran past 20s                               3
      failed to compile when called             31
    could not run                              425   (289 no writable inputs, 135 the interpreter refused)

"Compiles" is the front end, checker and lowering of what the entry reaches; a fn's
own codegen is exercised by the run, which is why 37 fns compile alone and hang when
called. Yesterday's 63% was against the April fork with the interpreter serving every
effect over a seam; this is the compiler as it is, with no seam, so the two are not
comparable. The Stdlib gap below is most of the difference.

## What blocks the 2,662

By the leaf cause, counting every fn that hangs off it:

    2,032  a Stdlib name the compiler's stdlib does not have
             LocalStore 341, Cli.Tui.TerminalSession 278, Cli.FileSystem 202, Env 120,
             HttpClient 109, Pretty 99, Cli.UI.Editor 89, Cli.Tui.Text 81, Cli.Unix 67,
             Cli.File 60, then Sqlite, Char, Cli.Daemon, HttpServer, DB
      230  a Builtin.* of ours (pm*, reflection, scm*) that has no native meaning
      224  the checker: an empty list literal typed List<t> against a declared List<Int>
             (34), a generic record alias not treated as a record (38, Stachu.Parser),
             non-exhaustive match on Stdlib.AltJson.Json (42, our arms against its
             cases), field access on a Result through a type variable (21), tuple
             pattern on an Option (6)
      131  lowering: lambda return type not inferable from an empty list body (90),
             a string literal as a list-cons head pattern (26), match in atom position
      34   the compiler does not finish in 60s (WIP.AI JSON builders: a non-tail
             if/match in a let is lowered by duplicating the continuation, 2^n)
      11   the front end (Dict<k, v> with a non-String key, a few let shapes)

## The 32 that differ

- Real candidates (2): `CompilerCases.Stdlib.Option.map_jsonStringLiteralPattern`
  (2 vs 1: a `Some` with a string-literal payload pattern) and
  `LanguageTools.NameResolver.namesToTry` (one extra candidate: its `"Stdlib" :: _`
  arm is taken for modules that do not start with "Stdlib", so a string literal as a
  list-cons head is matched as a wildcard where it compiles at all).
- Deliberate (1): `CompilerCases.Stdlib.Int64.shiftLeft_byWidth`, `1y << 8y` (masked
  shift count interpreted, 0 compiled).
- Float text (10): `Float.power`, `Float.sqrt`, `Math.atan/degrees/sin/sinh/tan/tanh/turns`.
  The compiler prints the shortest round-trip decimal, the interpreter 15 digits; the
  values agree.
- Host behaviour, not codegen (19): `Cli.Process.pipe/runIn/runWithEnv/shellPipeline/
  spawn` and `Cli.Tests.Instance.*` (the compiler's Cli returns Error where ours
  returns Ok with an exit code), `Cli.Path.resolve/resolveToAbsolute/relativeTo`
  (getcwd unavailable), and the nondeterministic `DateTime.now`, `Sys.currentPid`,
  `String.random`, `Uuid.generate`, `gid`, `gtlid`, `LetPattern.toPT`, `MatchPattern.toPT`.

## The 20 that crash

All SIGSEGV except `Cli.Docs.TerminalUI.content` (exit 1). One shape: a `match` whose
arm has a nested constructor pattern with a literal or record payload, on JSON or an
Option. The four `CompilerCases.Stdlib.Option.validateLike*` cases pin it (they were
written for the same crash in the April fork, where the fix was to test the tag before
loading the payload). The rest are its wild forms: `JsonRPC.Request.Parsing.
extractMethodFromRequest` and `validateJsonRpcVersionField`, `LLM.Providers.Anthropic.
extractUsage` and `OpenAI.extractUsage`, `PrettyPrinter.RuntimeTypes.RuntimeError.
toStringHint`, `SCM.PackageOps.bindingFromOp`, and nine `WIP.AI.*` parse/block fns.

## Authored cases

109 `Darklang.CompilerCases.*` fns: 101 match, 4 crash (`validateLike*`), 2 differ
(one deliberate, one real), 1 the interpreter refused, 1 does not compile.
