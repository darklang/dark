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

1. `StaticType` is the checker's internal type language. It separates inference
   variables from rigid declared type parameters.
2. `TypeEnvironment` is an immutable snapshot of type declarations and callable/value
   signatures. Storage and builtin adapters construct it outside the checker.
3. Conversion validates resolved names and declared type-variable scope. A recursive
   closure pass then validates custom-type presence and arity through every component
   a type can reach. Separate guards reject transparent alias cycles while allowing
   nominally recursive records and enums.
4. Unification applies substitutions, performs an occurs check, and treats custom
   types nominally. Aliases are expanded through `TypeEnvironment`, with cycle
   detection.
5. Bidirectional expression checking uses expected types where available and
   inference elsewhere. Immutable local and package values are generalized under a
   value restriction, and each reference receives a fresh instantiation. Deferred
   record-field constraints allow inferred local helpers to be checked at their call
   sites. Pattern checking both validates shape and extends the local environment,
   including recovery bindings after an invalid pattern, so one error does not create
   scope-error cascades. Inference variables confined to discarded intermediates or
   unused generic fields are erased at the item boundary; those in the observable
   result type, pending constraints, or provisional diagnostics remain blockers.
6. A package-batch layer predeclares signatures, validates declarations, then checks
   bodies. This permits mutually recursive functions without coupling checking to op
   replay or database order.
7. The authoring adapter walks resolved content hashes to load only the candidate's
   transitive dependency closure. Existing functions contribute signatures, not
   executable bodies. Serialized-input conversion probes the remaining call stack and
   dependency discovery uses an explicit work list, so pathologically deep input
   becomes a catchable `Incomplete` result instead of a process stack overflow.
8. Reports retain one verdict per content-addressed package reference as well as an
   aggregate verdict, so batch callers can map findings back to every location of an
   item without coupling the checker to mutable package names.

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

The gate is deliberately outside F# storage (`Builtin.scmAddOps`,
`Builtin.scmCommitWipOpsByIds`, `LibDB.Inserts`) and outside merge, rebase and sync,
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
enum cases must be matched exhaustively in code. A newly added AST case therefore
breaks compilation until its checking rule is chosen. Rules that cannot yet prove a
construct add a blocker and return an inference variable; they never silently accept
the construct.

Match exhaustiveness uses a constructor-matrix proof for unit, boolean, tuple, and enum
types, including nested and correlated patterns. Lists prove the common empty/cons
split. Infinite literal domains and guarded-only coverage remain conservative: when
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

## Where this should live

The checker is F# for throughput: it runs on every save, on every commit, and over the
whole corpus for `typecheck` and batch validation. That is an argument from the shape
of the work, not a measurement.

It should eventually be Darklang, and the reason is not tidiness. Today the set of
checks is fixed, and "our way is the way". The goal is that people can choose which
at-rest checks apply to their packages and write their own. That needs the checks to be
ordinary Darklang code, not an F# module with a builtin in front of it. There is a
`CLEANUP` marker on the module saying so.

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

    ./scripts/run-backend-tests --filter tests/AtRestTypeChecker
    ./scripts/run-cli typecheck

The test group has two parts. The `checker` unit tests build type environments by hand
and cover inference, mismatch diagnostics, patterns, enum and record validation,
aliases, missing dependencies, package-value ordering, the real builtin signatures, and
dependency-closure loading. The `authoring` test runs the real CLI against an isolated
seeded store and checks the rollout policy end to end: a definite failure is saved as
WIP but refused at commit, a checked declaration saves silently, and an incomplete one
saves with its warnings.

A full-corpus `typecheck` run is also a rollout gate, and a failure count alone is not
evidence that blocking is safe. Classify representative findings against the source
first, and add a regression test for every checker false positive found that way. As
inference coverage improves, declarations previously hidden behind `Incomplete` can
become definite failures; those are not automatically regressions. New `Failed`
declarations remain saveable as WIP, and `Incomplete` ones remain committable.
