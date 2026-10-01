# PR to pbiggar/darklang-compiler: what darklang/dark's package tree needs from the front end, the checker, pattern lowering, the inliner, tail calls and the driver

Branch `upstream-dark-fixes` in `~/code/compiler`, eighteen commits on `paul/main` (the
2026-09-15 tip). Not pushed. This page is the PR description as I'd post it, for review;
it supersedes the 2026-09-17 page, which covered the first three commits. The numbers in
the last section come from pushing darklang/dark's whole package tree through the
compiler with seventeen of them applied (the eighteenth, a list-arm guard, landed after the run and fixes the one real difference it found).

---

## Description

This makes the compiler accept the shapes darklang/dark's package tree is written in, and
fixes what the tree found once it got past the front end. It was found by pretty-printing
all ~5,200 package functions with their closures and compiling each from source with this
compiler and its own stdlib, then running the ones with writable arguments both ways.
Eighteen commits, one thing each; every one has an e2e or unit test that fails on main.
Grouped:

- Front end (3): let bodies and statement sequences from indentation; a function's bare
  name inside its own body; a nested match stays inside its arm, and a spaced minus is
  subtraction.
- Checker (3): an unsuffixed Int literal against a type variable; an empty list literal
  against an unbound type variable; a generic callee's type parameters freshened per call
  site rather than per index.
- Lambda lifting's inference (2): a return type across arms that a list literal leaves
  open; a body ending in a unary operator (`a != b` is `Not (a == b)`).
- Pattern lowering (4): a nested constructor pattern resolves in its payload's type; a
  payload is tested only after its tag matched; list-pattern heads are tested wherever
  the list pattern sits; a list arm's `when` guard is tested before the arm is taken.
- Inliner (2): a Bool callee with several returns is inlined through a join; the
  continuation copies for any other multi-return callee are bounded.
- Tail calls (1): a release of what a tail call borrows from stays after the call.
- Driver (2): a library unit may redeclare a function the prebuilt stdlib carries; the
  stdlib specializations a local generic's body reaches are requested in
  TestExpression mode.
- Stdlib (1): `Result.mapError` takes the result first.

Without the front-end three almost nothing in the tree compiles (a two-let function
misparses or spends 30s in the split-retry fallback). The pattern-lowering three and the
tail-call one are every SIGSEGV the sweep hit; the inliner two are every hang.

---

## The decisions worth a look

Layout is recovered textually, before lexing. The lexer discards columns and the parser
already had four pre-passes that put back what it needs (`in` after a lambda-valued
let, nested function boundaries, `val` boundaries, dedented match arms). I extended that
rather than making the lexer column-aware: the value-let pass now covers every value
let, and materializes a private statement separator (U+0002, lexed to
`TStatementSeparator`) between two lines at the indent of their block, which `parseExpr`
folds into `Sequence`. Nothing else learns about columns. The `continues`/`continued`
token lists in `insertValueLetLayoutSeparators` are a judgement about what can end or
start a continuation line; they cover the stdlib, your e2e corpus and the package tree,
and a shape I have not seen would be glued the old way, not a new way.

Type parameters are freshened per call site, with a counter. `freshenTypeParams` named
a callee's parameters by index, so every generic call in a body bound the same `a$0`,
and a generic seed passed to `Stdlib.List.fold` typed the result `Parser<Parser<a>>`.
The counter is module state, reset at the start of each function and each top-level
expression, so a program's names still depend only on itself (the two error messages
your suite pins are unchanged). With distinct names, a conflict between two open
bindings can be resolved instead of settled by order: `consolidateBindings` binds the
side whose variables inference owns (a freshened one, or `t$empty`, the element of an
untyped `[]`; a declared `'t` stays rigid) and keeps the other, and `reconcileTypes`
prefers the same direction. `emptyListElementVar` is the one spelling change: it was
`t`, which a declared `'t` could not be told from.

A pattern arm is lowered from stages, joined, not from one flat condition. The flat AND
of every load and test in an arm meant a variant's payload slot was loaded and examined
whether or not the tag matched; past a smaller variant that slot is whatever the heap
holds, and a nested tag load or a string compare through it is the SIGSEGV. Stages run
the tests in the order they are safe: a payload after its tag, a tuple's elements in
sequence, a list's length then heads then tail. Several stages become a Bool `Join`
whose entry jumps out with false at the first failure, so the rest of the match is
emitted once. My first attempt nested `If`s instead and copied the else branch per
stage, 2^n for n such arms, and hung on the stdlib build; that is why it is a join.
Or-patterns and the dedicated list-pattern compilers keep the flat comparison.

The inliner's growth is bounded two ways. `substituteReturn` splices the caller's
continuation into every `Return` of the inlined body, so a chain of multi-return callees
grows as the product of their return counts: the derived equality of a record with
fourteen Option fields was 76 million ANF nodes, and every pass after inlining looked
like a hang. A Bool callee with several returns now enters a `Join` whose continuation
is the caller's rest, each `Return` a `Jump`. Bool only, because a join carries an Int64
or a Bool and the interface verifier compares the jumped atom's inferred type, which an
Int64 temp typed by an unresolved variable does not satisfy. Any other multi-return
callee is inlined only when continuation size times (returns minus one) stays at or
under 64 nodes; otherwise the call is left as a call. The 64 is a guess that makes the
tree compile; I did not tune it.

Tail-call detection now knows what a temp borrows from. It moves the releases after a
call ahead of it, holding back any release of a temp the call passes; a projection was
not such a temp, so the closure read out of a record was called after the record's
release had run, and a record whose only field is a function is freed with its closure.
The pass carries borrow roots (`TupleGet`, `RecordGet`, `RawGet`, the `ToRawPtr` casts,
a `BorrowedCall`'s arguments, through aliases) separately from alias roots, so the
owned-parameter transfer for self tail calls does not mistake a projection for its
parameter. A blocked release keeps the call an ordinary call, as an argument's own
release already did.

`Result.mapError` is the one behavioural change to your stdlib: every other Result and
Option combinator takes the value first, the interpreter's `mapError` does too, and
`r |> Stdlib.Result.mapError f` relies on it. The pinned upstream `result.dark` cases
(in the disabled set) already spell that order.

---

## Changes made

In `src/DarkCompiler/frontend/Parser.fs`:

- `insertValueLetLayoutSeparators` replaces `insertLambdaLetLayoutSeparators`: the body
  of a `let` is the first line at or left of it (one column of tolerance for the test
  DSL's `(let`), explicit `in`s are paired with the `let`s on their line by position and
  nesting, a line ending in an operator or an opener, or starting with a closer, an
  operator or an arm, continues the expression above, and trailing `//` comments and
  string literals are ignored. `normalizeLayout` exposes the pass chain.
- `TStatementSeparator`, and `parseExpr` as a wrapper over `parseSingleExpr` that folds
  separated statements into `Sequence`.
- `preserveIndentedMatchBoundaries` looks through the whole arm body for a nested
  match, not only its first line, and `closeOnLastLine` puts the closers before a
  trailing `in` rather than on a line of their own after it.
- `TSpacedMinus`: `- ` followed by whitespace is subtraction only, accepted in
  `parseAdditive` and unary; `f -1` is unchanged. `Stdlib.List.length parts - 1` was
  passing `-1` as a second argument.
- Smaller: `TInterpString` as an application argument; `{{`/`}}` literal in an
  interpolated string; `parsePostfix` attaches an adjacent parenthesized constructor
  payload so `f Type.Variant(a, b)` passes one argument; `Type.Variant()` is a unit
  payload; a constructor's space-applied pattern payload is one pattern without a cons
  tail (`Number n :: rest` was `Number (n :: rest)`); `tryFunctionIndent` only considers
  a `let` that begins the line's code.

In `src/DarkCompiler/frontend/checking/`:

- `ResolveDeclarations.fs`: `resolveExpr` carries the enclosing top-level function as
  (bare name, declared name) and answers the bare spelling with the declared one, after
  locals, so a parameter of the same name still shadows it.
- `Expressions.fs`: an unsuffixed `Int` literal reconciles against a type variable the
  way the sized literals do; `[]` against an expected bare type variable types as
  `List<t$empty>` (that is every `Stdlib.List.fold xs [] f`, whose seed is checked
  against the still-unbound result type).
- `Diagnostics.fs`: `freshenTypeParams` numbers call sites; `resetFreshening` is called
  from `Functions.fs` per function and `ResolvedProgram.fs` per top-level expression.
- `Unification.fs`: `isInferenceVar`, the conflict rule in `consolidateBindings`, the
  transitive apply in `inferTypeArgs`, and the direction preference in `reconcileTypes`.

In `src/DarkCompiler/passes/preparation/ClosureAnalysis.fs`: `simpleInferType` infers a
list literal (List of its first element, open when empty), a `UnaryOp` (Not is Bool,
Neg and BitNot take the operand's type), and the branch reconciliation `if` already had
is a top-level `reconcileBranchTypes` that descends through lists, tuples, records,
dicts and functions and is used by `match` too, which required every arm to agree
exactly.

In `src/DarkCompiler/passes/anf/lowering/PatternLowering.fs`:

- `buildPatternComparison` takes the static type of the value it tests (the enclosing
  variant's payload with type arguments substituted, a tuple's element, a list's element
  or tail, the scrutinee at the top), so `| Some(String _)` on an `Option<Json>` looks
  `String` up in `Json`, not in whatever type registered a `String` variant last.
- `buildPatternStages` and `stagesToIf`: the staged lowering above, used by `buildChain`
  for single-pattern arms with a testing nested pattern, and for list patterns below the
  top of an arm (in a tuple or a payload), which were one length test with the heads
  never looked at (`("Stdlib" :: _, x)` matched every non-empty list). A string or
  constructor head at the top of an arm, which was "Unsupported head pattern in list
  cons", goes through the stages too. `collectPatternBindings` is hoisted out of the
  tuple case, unchanged.
- A statically dead last arm still compiles its body (the "Missing constructor payload
  type" unit test wants the error from inside it).
- A list or list-cons arm with a `when` guard that is not the last arm goes through the
  stages rather than the specialized list compilers, which took the body without the
  guard: `flag :: _ when Stdlib.String.startsWith flag "--"` matched every non-empty
  list. The last arm already tested its guard.

In `src/DarkCompiler/passes/anf/ANF_Inlining.fs`: `countReturns`, `returnsToJumps`, the
Bool join when `info.Func.ReturnType = AST.TBool` and the body has more than one return;
`continuationSize` and `maxContinuationCopy = 64` for the rest.

In `src/DarkCompiler/passes/anf/TailCallDetection.fs`: `borrowSources`,
`extendBorrowRoots`, and `tailCallArgTempIds` taking the borrow roots into the set a
movable release may not touch.

In `src/DarkCompiler/driver/`:

- `UserCompilation.fs`: `mergeFunctionsByName` keeps the prebuilt copy for any name the
  reachable stdlib supplies, not only `Stdlib.*` and the helpers. Your stdlib carries
  `Darklang.LanguageTools.PackageManager.countMatchingPrefix` and
  `ProgramTypes.hashToString`, and a library unit compiled from that package tree
  redeclares them with a different lowering.
- `SourcePreparation.fs`: in TestExpression mode the dependency planning specializes
  the local generics first and adds what their specialized bodies reach
  (`specializeFromSpecs` already reports it as `ExternalSpecs`) to the stdlib
  specializations it requests. A fold over `Parser<a>` inside `choice<'a>`, called as
  `choice<String>`, otherwise reaches lowering as "Missing specialization for
  `Stdlib.List.fold<Parser<str>, ...>`". FullProgram mode did not have this.

In `src/DarkCompiler/stdlib/Result.dark`: `mapError (result) (fn)`.

Tests, in `src/Tests/e2e/`: `interpreter_layout_parity.e2e` (one case per front-end
and checker shape), `nested_pattern_guarded_payload.e2e` (the three payload shapes and
the list heads), `inline_join_growth.e2e` (the fourteen-field record equality, the
ten-Option-field serializer, and the inference cases, which came out of the same fns),
`generic_call_freshening.e2e` (the fold seed, the nested generic call, the tuple seed),
`tail_call_borrowed_callee.e2e` (the closure and the list argument shapes),
`list_arm_guard.e2e` (the cons, literal-head and fixed-length shapes); and in
`src/Tests/compiler-passes/ProgramStructureTests.fs`, the redeclaring library unit and
the generic library unit.
`docs/compatibility/language/bindings.md` says what `let` layout now means.

Run with `--e2e-batch-size=64`; the default batch size overflows the stack in this
container on main as well.

---

## What it is worth

Numbers from darklang/dark's package tree, every function pretty-printed with its
closure and compiled from source in TestExpression mode, then run both ways where
arguments could be synthesized (the report in that repo has the per-function
breakdown). Before, in parentheses, is the same sweep with the first three commits;
before those a two-let function did not parse and the sweep was not worth running.

    package fns                                  5,174
    compile (front end, checker, lowering)       2,770   54%    (2,509, 49%)
      Stdlib fns the compiler's stdlib has         927 of 1,354
      other fns not blocked by a Stdlib gap      1,843 of 1,970   94%   (80%)
    run both ways on synthesized inputs          2,423
      identical JSON                             2,361   97%    (1,961)
      differ                                        31   (1 real, the guard; 1 deliberate; 9 float text; 20 host or nondeterministic)
      crash                                          1   (20)
      past the 60s budget                           22   (37 hung when called)
      failed to compile when called                  8   (31)

Of the 2,404 that do not compile, 2,037 hang off a Stdlib name your stdlib does not
have and 240 off a `Builtin.*` of ours; 127 are the compiler's (72 checker, 23 past
60s, 20 lowering, 12 front end), listed below.

---

## Not in this PR

What the same sweep finds with these applied, in the order it blocks package
functions; these are compiler gaps rather than front-end ones and I did not want to
guess at your design for them:

- The stdlib surface: about 2,000 fns hang off a `Stdlib.*` name the compiler's stdlib
  does not have (LocalStore, Cli.Tui.TerminalSession, Cli.FileSystem, Env, HttpClient,
  Pretty, Cli.UI.Editor, Cli.Tui.Text, Cli.Unix, Cli.File, Sqlite, Char, ...). These
  are effects and a display library, not compiler gaps, and I have not added any of
  them; the ledger already lists most of them as absent. `Stdlib.List.singleton` (7
  fns) is the one pure omission.
- "Constructor identity collision": the 12-bit constructor tag is a hash of
  `Type.Case`, and two types sharing a case name collide once a program has enough
  types (`Cli.Packages.PackageLocation.Type` against another `Type`, a
  `LetPattern` case against another). The whole Cli closure hits it.
- FullProgram mode's code generation is superlinear in output size. A closure of
  23 KB of source (`SCM.Draft.addedHashes` with what it reaches) compiles in 7s in
  TestExpression mode, where tree shaking leaves a 632-byte binary, and in 383s in
  FullProgram mode, where the binary is 2.4 MB and `[backend.codegen] Code
  Generation` alone is 381s of it; every other pass is under a second. The sweep
  saw 32 such closures pass a 300s budget in FullProgram mode, so it stays on
  TestExpression mode; the units are saved for a reproducer.
- OutOfMemory in the compiler on the largest closures (`Cli.Workbench`), and about 20
  fns whose closure is the whole `ProgramTypes.Expr` pretty-printer or tokenizer run
  past 60s even in TestExpression mode; not measured further.
- The e2e harness's preamble path crashes in `rcShapeOfTypeWithSums` ("Record type
  'X' not found in typeReg") when a preamble function folds over a record with a
  function-typed field, seeded by a call: `Stdlib.List.fold ps (fail "x") (fun acc p ->
  orElse acc p)` with `type Parser = { run: String -> Result }`. The same program
  compiles and runs as a file. The test for the freshening commit is written around
  lists for this reason.
- An enum pattern with fewer fields than the case has (`| SetName(loc, reference) ->` on
  a three-field case) is accepted by the checker and fails in lowering as "Undefined
  variable: loc". The interpreter never matches such an arm; a checker error would be
  kinder. (That one was a bug in our tree, now fixed there.)
- `Stdlib.Int8.shiftLeft 1y 8y` is 0; the interpreter masks the shift count and gives 1.
- The checker, smaller: a fold whose lambda returns a `Result`-typed accumulator is
  refused against the seed's type (12 fns, `NameResolver.TypeName.resolve`); a
  `branchId: Uuid` parameter reported as String where the callee is a `Builtin.*`
  (7); a tuple pattern in a lambda whose parameter type is still a variable at
  lifting (8, `Stachu.DarklangParser.infixExpr`); a lambda whose only arm is
  `acc + x` over an untyped `[]` (7).
- Lowering, smaller: "Could not recover List type from intrinsic __rawptr_to_list_..."
  (6, WIP.AI.OpenAI.Chat.parseToolCalls); "argument count mismatch when inlining" (4,
  Cli.Docs.Command.Topics.allTopics); "If expression requires lazy branch lowering"
  and "Match expressions in atom position" (3 each); an x86-64 "Undefined label:
  __dark_list_rc_dec_plan_..." when four JSON getters are called.
- A `Dict<k, v>` with a non-String key is a parse error (12 fns, with a few let
  shapes).

I can open issues for each with a reproducing package function if that is useful.
