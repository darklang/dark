# At-rest type checker

The at-rest type checker validates serialized `ProgramTypes` without executing them.
Its first responsibility is to stop definite type errors from surviving until a code
path happens to run. Its longer-term responsibility is to provide a proof boundary
that the interpreter can trust when deciding which redundant runtime checks to omit.

## Soundness contract

The checker has three outcomes:

- `Checked` means the complete item was checked against a closed, immutable type
  environment. Every referenced type, value, and function was present, every AST
  node was handled, and all generated type constraints were solved.
- `Failed` means at least one definite type error was found. Diagnostics are
  structured and carry stable codes and expression or pattern IDs. A `Failed` item
  may also have blockers: an unrelated incomplete proof does not hide a concrete
  error.
- `Incomplete` means the checker could not prove the item safe. Missing dependencies,
  unresolved names, unsupported constructs, alias cycles, and ambiguous inference all
  belong here. It contains no definite diagnostic and must never be treated as
  `Checked`.

Checking is pure and deterministic for a given item and type environment. It does not
evaluate user code, query mutable storage, format diagnostics for a particular UI, or
mutate package state.

## Architecture

The checker is Darklang, in `packages/darklang/languageTools/atRestTypeChecker/`, behind
`LanguageTools.AtRestTypeChecker.checkPackageOps` and `checkBranch`. Authoring, commit,
propagation, the LSP and `typecheck` all go through those two. It works in three steps,
one module each:

1. `Generate` turns an item's syntax into `Constraint`s: one rule per construct, no
   lookups, no state. Unknowns are named after the node that introduced them
   (`Of(nodeId, Element)`), so no counter is threaded through it. Two nodes sharing an
   id only add equalities, which can make an item fail but never pass.
2. `Solve` works through the constraints in order against an `Environment`, keeping a
   substitution. Order carries the bidirectional part: a callee's type is known before
   its arguments are checked, so a lambda argument knows its parameter types. A
   constraint about something not yet known (whose field is this, which enum has this
   case, is this operand numeric) waits and is retried as the rest of the body fixes
   types; whatever is still waiting at the end is a blocker, never a pass. Immutable
   local and package values are generalized under a value restriction; waiting
   constraints and exhaustiveness checks about generalized unknowns travel with the
   scheme and are re-checked for each instance. An unknown that a waiting constraint
   ties to one still in scope is not generalized, since that constraint stays behind
   and a fresh instance would escape it. A `TError` type stands for "already reported"
   or "cannot be known": it unifies with everything, so one mistake is reported once,
   and an alias that cannot be expanded (a cycle, or a declaration that is not
   available) becomes one, with its blocker, rather than a mismatch the checker has no
   evidence for. Function types keep their arity, as the runtime's own check of a
   function against a declared type does, so a call whose callee is not known yet
   waits to learn it rather than guess a shape: `fun g -> g 1 2` may be given a
   two-parameter function or one that returns a function. A waiting call still
   says its callee contains what it is applied to and what it gives, so calls that
   would make a function contain itself, directly (`fun x -> x x`) or through
   another waiting call (`fun f g -> (f g, g f)`), are reported when they start to
   wait. Calls sharing an unknown callee unify their common argument prefixes and,
   for equal argument counts, their results. A longer call applies the shorter
   call's result to its remaining arguments, without fixing the callee's arity.
   Waiting constraints are retried while any are resolved, any argument is consumed,
   or any unknown is decided. Residual applications go through the same checks.
   Deferred reads and updates of the same field, unwraps of the same subject, and
   patterns for the same enum case also reconcile their types before generalization.
   Every waiting application enters that same reconciliation path. Requirements for
   records, enums, numbers and functions are disjoint; their intersections are checked
   even when the subject is unknown. Unwraps contribute structural containment edges
   alongside applications, and an enum pattern can resolve their Option/Result choice.
   Dictionary-key requirements follow observed fields, enum payloads and unwrap results,
   including through nested containers and aliases; phantom arguments remain exempt.
   This reachability walk has its own visited set, since nominal recursive fields are
   legal even though structural cycles such as `x = Option<x>` are not.
   Their original source sites remain queued for checking once the declaration is
   known; repeated settlement does not duplicate a relational diagnostic.
   Field access, record update and enum patterns on a type known not to be custom
   (`Int`, a list, a function) are definite errors; on an unknown or a type
   parameter they stay open. These diagnostics carry explicit certainty, so an
   unknown list element type does not downgrade an invalid field access to ambiguity.
3. `Items` decides each verdict under the soundness contract above. Unknowns confined to
   discarded intermediates are erased; those in the item's type or needed to decide
   a diagnostic remain, as blockers or as provisional (not definite) diagnostics.

Around them:

- `Types` holds what needs no solver: conversion from `TypeReference`, alias expansion,
  Dict-key usability, and well-formedness. Work that depends only on a declaration or
  a signature is done once per batch and cached in the `Environment`: each
  declaration's own problems and references, each one's closure of problems (computed
  over strongly connected components, so it is linear in the declaration graph), alias
  cycles (one pass over the alias graph), and each called function's signature,
  converted and checked once and instantiated per call.
- `Coverage` proves match exhaustiveness and renders a witness for the message.
- `Environment` builds what an item is checked against: builtin signatures, taken from
  the runtime lazily as items call them, and the dependency closure, loaded by content
  hash through the package manager. Existing functions contribute signatures, not
  bodies. A builtin a declaration calls contributes the types its signature names.
  Each dependency's retrieval and reference walk share one guard, so a failed load
  leaves that dependency unavailable without dropping the rest of the queue.
- `Run` checks a batch: types and function signatures are declared first, so order and
  mutual recursion do not matter; values are checked in dependency order, and values in
  a cycle stay `Incomplete`. Reports keep one verdict per content-addressed item.

Dark code cannot catch its own runtime error, so the checker runs its pieces through
`AtRestTypeChecker.guard` (`Builtin.atRestCheckGuarded`), which hands back a failure of
the checker itself as a `CheckFailure` value: `TooDeep` for input nested deeply enough to
exhaust the native stack (recursion through builtin callbacks, see
`Execution.runLoaded`, or AST conversion, see `ProgramTypesToDarkTypes`), `Failed` for
anything else. AST conversions probe the stack in both directions, including when a
stored dependency is converted into Dark values. Each item is guarded on its own, from
walking its body for references to checking it, and a failure there makes that item
`Incomplete` with a `DeclarationTooDeep` or `CheckerUnavailable` blocker while the rest of
the batch is still checked, so one deep declaration cannot hide another's definite
error. Both entry points are guarded as a whole as well, for what the batch shares, like
loading its environment or querying the branch catalog. Branch checking asks for names
and hashes first, then guards each candidate's retrieval and conversion separately.
Shared declaration closures deduplicate findings with guarded equality, not the native
structural hashing in `List.unique`. If comparing findings exceeds the depth limit,
deduplication keeps the original findings instead of failing the shared environment.
The checker therefore preserves other items' results when an item's analysis or
conversion exceeds the supported depth. A catalog or other shared-infrastructure
failure still produces a whole-batch `Incomplete` report.

The `Invariants` module in
`backend/testfiles/execution/stdlib/language-tools/atRestTypeChecker.dark` exercises
these boundaries together: incompatible operation pairs in both orders, delayed
subject equality, valid overlaps, dictionary-key reachability, structural cycles,
deep duplicate findings, and mixed successful/failed/missing candidate loads. The
native `DeepValues` and `DeepProgramTypes` tests cover comparison, type merging, and
the outbound AST converters on a small stack. These complement the individual syntax
and inference rules elsewhere in the checker test file.

## Trust boundary and rollout

Authoring warns; commit blocks.

`SCM.PackageOps.addAuthored` (the `fn`, `type`, `val` and `module` commands, the
Workbench save path, and the LSP filesystem provider) stabilizes hashes, stores the
batch as WIP whatever the checker says, and returns the report for the surface to
show. WIP is the author's to break, like a working tree.

`SCM.PackageOps.commit` / `commitOpIds` re-check the committing ops as one batch and
refuse a `Failed` verdict, so a definite type error never leaves a branch.
`commit --allow-type-errors` commits anyway; `--force`, which skips the
unresolved-references check, does not. Re-checking at commit rather than trusting the
save-time report matters because WIP moves: fixing `g` is what un-fails `f`. `Checked`
and `Incomplete` both commit freely, since an incomplete proof means the checker
lacked evidence and found no definite error. An adapter failure is itself an
`Incomplete` report, so a rollout defect cannot make authoring or committing
unavailable.

`SCM.PackageOps.add` is raw storage, with no check and no rejection, for ops that
carry final hashes and add no declaration: sync, rename, deprecate.

Updating a definition rewrites its dependents with a blind hash swap, so a signature
change can materialize broken callers without a word said. The CLI's propagation path
therefore re-checks the current bodies of all visible transitive dependents right
afterwards and prints the findings, advisory like the save-time report. Transitive
checking is what matters here: a direct dependent can stay valid while its inferred or
expanded type changes and breaks its own callers. Failed dependents are listed as
having errors after the update, without claiming the update caused them.

The read-only `typecheck` CLI command checks every visible type, value, and function
on the current branch in one batch, printing aggregate counts and listing non-checked
items by location. `--all`, `--failed` and `--incomplete` filter the detail. It adds no
package operations and mutates no branch state.

The gate is deliberately outside storage (`Builtin.scmAddOps`,
`LibDB.Inserts`, the `SCM.Commits` commit path) and outside merge, rebase and sync,
which move already-committed content. Package synchronization, historical op replay,
propagation, and other storage callers never see it and must not reject data based on
this checker. Persistence, when added, stores regenerable verdicts keyed by the item
hash and checker version.

Runtime check elision is a separate rollout. It requires a `Checked` proof for the
complete dependency closure under the same checker version. `Failed`, `Incomplete`,
missing, or stale proofs always retain current runtime checks.

## Editor diagnostics

The language server runs the checker for syntax-clean documents on open, full-document
change, and save. It publishes definite diagnostics as LSP errors and blockers as LSP
warnings, per issue: an item that is `Failed` and also has blockers shows one error and
one warning, not two errors. Diagnostics carry the checker issue code and use
`darklang-at-rest` as their source. They clear as soon as the document checks cleanly
and when it is closed. Parser errors continue to be reported while an edit is
syntactically incomplete; the at-rest checker waits for a clean syntax tree.

Checker node IDs are not source locations. Until lowering carries stable source
locations into `ProgramTypes`, an issue is attached to its containing declaration body
(or to the type declaration for type issues). That is exact for a single-expression
body such as `let invalid (x: Int) : String = x`, and conservative for anything more
deeply nested.

## Coverage policy

All `ProgramTypes.Expr`, let-pattern, match-pattern, pipe, type-reference, record, and
enum cases are matched exhaustively in the checker, with no wildcard arm standing in for
a case. Darklang does not check exhaustiveness at compile time, so a newly added AST case
is not a build error: until its rule is written, meeting it is a "No matching case"
runtime error inside the checker, which the guard turns into an `Incomplete` report.
That is safe, since nothing unproven is ever `Checked`, but it is silent until someone
meets the case; add the rule, and a case for it in the checker's tests, in the same
change as the new syntax. Rules that cannot yet prove a construct add a blocker and
return an unknown; they never silently accept it.

Match exhaustiveness uses a constructor-matrix proof for unit, boolean, tuple, and enum
types, including nested and correlated patterns. Lists prove the common empty/cons
split. A column's constructors are expanded only when its patterns name every one of
them, and otherwise only the rows that match anything decide; expanding regardless
does not terminate on a recursive type, where a wildcard specialized to `Node of Tree`
is the same row again. Infinite literal domains and guarded-only coverage remain conservative: when
complete coverage cannot be proved, the expression is `Incomplete`.

Builtin signatures are part of the trust boundary. Concrete runtime results use their
actual package types, and ordinary generic builtins declare or structurally expose
their type variables. Two kinds of signature must not be trusted:

- A result type variable that no parameter constrains (`Hash -> Option<'a>`,
  `optOrRes -> 'a`) means the result is only known at runtime. The checker detects this
  from the signature and treats the builtin as unsupported rather than quantifying the
  variable. No builtin is recognized by name for this.
- The operator builtins (`add`, `lessThan`, `equals`, `negate`, ...) declare
  independent `'a`/`'b` parameters because the type language has no numeric constraint,
  but raise at runtime on anything but values of the same numeric type. They are what
  infix syntax lowers to (`PT.InfixFnName.toBuiltinName`; the parser lowers `-x` to
  `negate`), so `a + b` and `Builtin.add a b` execute identically, and the checker
  checks a by-name call with the operator's rule read from that same table rather than
  with the declared signature. Operator domains follow the runtime operation, not one
  universal numeric set: `power` excludes `Int128` and `UInt128` even though other
  arithmetic supports them. Used as a value or partially applied there is no signature
  to give them, and the use is `Incomplete`.

## Why Darklang

The checker was F# until it was rewritten in Darklang, and the reason was not tidiness.
With an F# module behind a builtin, the set of checks is fixed. The goal is that people
can choose which at-rest checks apply to their packages and write their own, which needs
the checks to be ordinary Darklang code.

The rewrite was held to the F# checker over the whole package tree before it replaced
it: on about 6,600 items they disagreed on 11, all in the Darklang checker's favour (9
items F# left `Incomplete` for ambiguity that waiting resolves, and 2 failing under both
where it also reported problems inside fields F# had stopped checking). It never passed
an item F# failed or failed one F# passed.

The price is throughput. Over the whole branch the F# checker took about 10s; the
Darklang one takes about 45s. A save or a commit checks one declaration and its
dependencies, which takes a few milliseconds to a few hundred. The time is spread
thinly, at roughly 1,500 interpreted calls per function checked, so it tracks the
interpreter's speed. Two things got it here and are worth keeping: work that depends
only on a signature or a declaration is done once per batch, not per call site or per
item; and the modules name types in full rather than through `type X = ...` aliases,
which miss the interpreter's fast argument check (see `model.dark`).

## Non-goals

- Replacing name resolution or parsing.
- Executing constants to discover their types.
- Rejecting synchronized or historical package operations.
- Inferring public function signatures; package functions already declare them.
- Treating runtime values as static evidence without a declared or trusted signature.
- Persisting source ranges in `ProgramTypes`. Editor diagnostics use the corresponding
  `WrittenTypes` declaration range; precise nested-expression mapping remains a
  separate lowering concern.

## Verification

    ./scripts/run-backend-tests --filter tests/LibExecution/All/testfiles/execution/stdlib/language-tools/atRestTypeChecker
    ./scripts/run-cli typecheck

`backend/testfiles/execution/stdlib/language-tools/atRestTypeChecker.dark` pins the
checker down rule by rule, with every issue code covered. Cases are written as source
where the parser can say them (declarations in one snippet can refer to each other,
through `LanguageTools.Parser.Parse.packageSourceToOps`) and hand-built where it cannot:
unresolved names, missing hashes, or-alternatives that bind different names. Deep input
is tested through the guarded entry point: it either checks or comes back `Incomplete`
as too deep to walk, and never takes the process down. The rollout policy is tested end
to end against the real CLI in `CliScm.Tests.fs`: a definite failure is saved as WIP but
refused at commit, and `--allow-type-errors` takes it.

A full-corpus `typecheck` run is also a rollout gate, and a failure count alone is not
evidence that blocking is safe. Classify representative findings against the source
first, and add a regression test for every checker false positive found that way. As
inference coverage improves, declarations previously hidden behind `Incomplete` can
become definite failures; those are not automatically regressions. New `Failed`
declarations remain saveable as WIP, and `Incomplete` ones remain committable.
