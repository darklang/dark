# At-rest type checker

The at-rest type checker checks stored `ProgramTypes` without running user code. It reports structural and type errors independently of execution. `run` and `eval` do not invoke it or block on its findings; runtime checks apply when code executes.

## Check results

The checker has three outcomes:

- `Checked`: the whole item passed in a closed, immutable type environment. Every referenced type, value, and function was available, every AST node was handled, and all generated type constraints were solved.
- `Failed`: the checker found a type error. It may also report parts it could not check. Each error has a stable code and an expression or pattern ID.
- `Incomplete`: the checker found no type errors but could not finish checking. Reasons include missing dependencies, unresolved names, unsupported features, circular type aliases, or types it could not infer. This is not a pass.

The report separates errors the checker is sure about (`diagnostics`) from things it could not verify (`warnings`). A report can include both, even when the result is `Failed`.

Checking the same code with the same type information always gives the same result. The checker does not run user code, read from mutable storage, change stored packages, or format messages for display.

## Architecture

The checker is written in Darklang and lives in
`packages/darklang/languageTools/atRestTypeChecker/`. It exposes two functions in `LanguageTools.AtRestTypeChecker`:

- `checkPackageOps` checks a batch of declarations.
- `checkBranch` checks all visible declarations on a branch.

Authoring, commits, propagation, the LSP, and `typecheck` use these functions.
Checking follows parsing and name resolution.

Three modules do the core work:

1. `Generate` creates type constraints from syntax, using one rule for each construct. It does not look up declarations or keep state. Types that are not yet known get an ID based on their syntax node and role. For example, `Of(nodeId, Element)` identifies a list's element type. This avoids needing a counter to assign IDs.
2. `Solve` processes constraints in order against an `Environment`, maintaining a substitution. A callee's type guides argument checking, including lambda parameter types. Constraints that need more information wait and are retried. Any still waiting at the end become warnings.
3. `Items` assigns the verdict. It erases unknowns confined to discarded intermediates. Unknowns in the item's type, or needed to decide a diagnostic, remain as warnings or provisional findings.

`NodeIds` rejects repeated inference IDs before generation, making the body
`Incomplete`: collisions could otherwise lose constraints during generalization.
Generalized let bindings also use pattern IDs as scheme keys. A repeated binding ID produces an unsupported-construct warning and discards its scheme, preventing it from replacing another binding's type and causing a false pass or mismatch.

### Solver rules

Immutable local and package values are generalized under a value restriction.
Waiting constraints and exhaustiveness checks on generalized unknowns travel with the type scheme and are checked for each instance. Renaming and resolving
their types share one traversal, preserving constraint order and diagnostic sites. An unknown tied by a waiting constraint to another unknown still in scope cannot be generalized: a fresh instance would escape the constraint left behind.

`TError` means "already reported" or "cannot be known". It unifies with any type to avoid duplicate errors. An alias that cannot be expanded, because of a cycle or a missing declaration, becomes `TError` with a warning.

Function types preserve arity, matching runtime checks against declared types. A call with an unknown callee waits for its type. For example, `fun g -> g 1 2`
can accept a two-parameter function or a function that returns another function.
Waiting calls still constrain their argument and result types:

- Structural containment checks reject types that would contain themselves, both directly (`fun x -> x x`) and across calls (`fun f g -> (f g, g f)`), as soon as those calls begin waiting.
- Calls sharing an unknown callee unify their common argument prefixes. Calls with equal argument counts also unify their results. A longer call applies the shorter call's result to the remaining arguments without fixing the callee's arity.
- Constraints are retried while any constraint resolves, argument is consumed, or unknown is decided. Residual applications receive the same checks.

Before generalization, the solver also reconciles deferred reads and updates of the same field, unwraps of the same subject, and patterns for the same enum case. Every waiting application uses this same reconciliation path. Record, enum, numeric, and function requirements are disjoint; incompatible combinations
are rejected even when the subject's type is unknown. Unwraps add structural containment edges, like applications. An enum pattern can determine whether an unwrap uses Option or Result.

Postfix `?` unwraps a value and can return early. The innermost function or lambda must return the same Option/Result kind, with the same error type for Result. A known operand immediately determines the extracted type. The return side is checked once known (after the body for a lambda), and mismatches are reported at `?`. Either side can determine the kind; a lambda with no other return-type constraint takes the operand's kind.

Dictionary-key requirements follow observed fields, enum payloads, and unwrap results through nested containers and aliases. Phantom type arguments impose no requirement. This reachability walk has its own visited set: nominal recursive fields are legal, but structural cycles such as `x = Option<x>` are not.
Original source sites stay queued until the declaration is known; repeated settlement does not duplicate relational diagnostics.

Field access, record updates, and enum patterns on incompatible types are definite errors. Declared type parameters must work for every instantiation; inference variables can wait for a caller's type. Incompatible outer types or arities are definite errors even when nested types are unknown.

### Supporting modules

- `Types` converts `TypeReference`s, substitutes types, expands aliases, and checks Dict-key usability.
- `Declarations` validates types and caches their own problems, references, alias cycles, and transitive problems in the `Environment`. Strongly connected components avoid repeated walks through shared dependencies.
- `Coverage` proves match exhaustiveness and produces a witness for the message.
- `Environment` loads the dependency closure by content hash through the package manager. Package functions must declare their public signatures. Existing functions contribute signatures, not bodies. Builtin signatures are fetched lazily by name from the runtime, including the types they reference, without materializing the full registry for each check. Called signatures are converted and validated once per batch, then instantiated per call. Dictionary-key requirements wait for inferred arguments and travel with generalized local and package values. Each dependency's retrieval and reference walk share a guard: a failed load marks it unavailable and preserves the rest of the queue.
- `Run` checks batches. It declares types and function signatures first, supporting any order and mutual recursion. Values are checked in dependency order; cycles remain `Incomplete`. Package tests are checked last, against the environment holding the batch's value schemes: nothing can reference a test, so no other item waits on one. An ordinary test's body must return `Stdlib.Test.Result`, which is loaded even when the body never names it; a `=> raises` test is only observed through its runtime error, so its body is inferred with no expected type. Reports contain one verdict per content-addressed item.

`Declarations` computes dictionary-key requirements to a finite fixed point, including requirements inherited through aliases, records, enums, and recursive declarations. Each use checks its actual type arguments; phantom parameters impose none. Value reachability uses a least fixed point: passing a parameter around a recursive cycle does not make it a runtime value. There must be a path to a payload, including paths that permute type arguments.

Work that depends only on a signature or declaration is cached once per batch. The runtime also caches type-alias resolution, so there is no need to spell out full type names for performance.

### Runtime boundary and deep inputs

The type-checking rules run in Darklang. F# runs the interpreter, converts stored ASTs to and from Dark values, and catches failures Dark code cannot catch. `AtRestTypeChecker.guard` calls `Builtin.atRestCheckGuarded` for this protection. The adapter does not infer types or judge whether user code is well typed.

The checker can reach the native stack limit. Recursing inside
`Stdlib.List.map`, for example, starts a nested interpreter run at each level.
F# AST conversion and value comparison also use the native stack. These runtime paths check available stack space so the guard can return `CheckFailure.TooDeep`. Other checker runtime errors become
`CheckFailure.Failed`. Neither is a definite type error in the input.

Each item's loading, conversion, reference collection, and checking are guarded.
Failure makes that item `Incomplete`, with a `DeclarationTooDeep` or
`CheckerUnavailable` warning; other items keep their results. Branch checking fetches names and hashes first, so one item's conversion cannot abort the batch.
An outer guard covers shared work, such as environment loading and catalog queries. Failure there makes the whole batch `Incomplete`.

Shared declaration findings use guarded equality for deduplication. If
comparison exceeds the depth limit, the original findings are kept. Native structural hashing via `List.unique` could let one deep finding abort environment construction.

For deep Dark checker walks, use direct recursion or an explicit work list.
Avoid recursion inside builtin callbacks. Stack checks belong in the F# runtime paths above.

## Trust boundary and rollout

Authoring saves with warnings. Commit rejects definite errors unless overridden. Script execution does not run the at-rest checker. Runtime checks remain enabled.

### Scripts and eval

Scripts and `eval` parse and resolve names, then execute under guest permissions.
They do not run the at-rest checker. An unused function with a type error does
not prevent execution; a reached runtime error stops the run after any earlier
effects. Name resolution uses the selected branch and the script's declarations.
Top-level expressions except the last must return Unit, enforced at runtime.

Script value initializers run under guest permissions before top-level
expressions. Results are cached per run, including forward dependencies reached through functions. An error or dependency cycle stops execution. Stored package values use their already evaluated data.

Runtime rules check postfix `?`. A direct return annotation or final Option/Result constructor enables the check for mixing Option and Result; it does not expand aliases or follow helper calls.
Declared functions also check their final return value, including aliases.
Lambdas have no declared return-type check.

### Authoring and commit

`SCM.PackageOps.addAuthored` stabilizes hashes, stores the batch as WIP regardless of the verdict, and returns the report for display. It serves `fn`, `type`, `val`, `module`, Workbench saves, and the LSP filesystem provider.

`SCM.PackageOps.commit` and `commitOpIds` re-check the committing ops as one batch and refuse `Failed`. `commit --allow-type-errors` overrides this;
`--force` only skips the unresolved-references check. Re-checking matters because WIP changes: fixing `g` can make `f` pass. `Checked` and `Incomplete` both allow commit. Adapter failures produce `Incomplete`, so an adapter failure alone cannot block a save or commit.

Updating a definition replaces hashes in its dependents without checking types.
The CLI therefore re-checks the current bodies of all visible transitive dependents after propagation and reports findings. These reports are advisory.
Transitive checking matters: a direct dependent can remain valid while its inferred or expanded type changes and breaks its callers. Reports say which dependents have errors after the update without claiming the update caused them.

The read-only `typecheck` command checks every visible type, value, and function on the current branch in one batch. It prints totals and lists non-checked items by location. `--all`, `--failed`, and `--incomplete` control the detail. It adds no package operations and changes no branch state.

### Storage and future runtime checks

`SCM.PackageOps.add` stores ops with final hashes that add no declaration, such as sync, rename, and deprecate, without checking or rejecting them.

The gate stays outside storage (`Builtin.scmAddOps`, `LibDB.Inserts`, and the `SCM.Commits` commit path) and merge, rebase, and sync, which move already-committed content. Synchronization, historical replay, propagation, and other storage callers must not reject data based on this checker. Future persisted verdicts will be regenerable and keyed by item hash and checker version.

Skipping runtime checks is a separate rollout. It requires a `Checked` proof for the complete dependency closure under the same checker version. Failed, incomplete, missing, or stale proofs always keep runtime checks enabled.

## Editor diagnostics

The language server checks syntax-clean documents on open, full-document change, and save. Definite diagnostics become LSP errors; checker warnings become LSP warnings, even when they belong to a `Failed` item. Each carries its issue code and the source `darklang-at-rest`. Diagnostics clear when the document checks
cleanly or closes. During syntax errors, the parser reports diagnostics and the checker waits.

Checker node IDs are not source locations. Diagnostics use declaration locations from `WrittenTypes`. Until lowering preserves stable source locations in `ProgramTypes`, issues point to the containing declaration body, or the type declaration for type issues. This is exact for a single-expression body such as `let invalid (x: Int) : String = x`, but only approximate for nested expressions.

## Coverage policy

Every `ProgramTypes.Expr`, let-pattern, match-pattern, pipe, type-reference, record, and enum case has an explicit checker arm. Darklang does not check exhaustiveness at compile time. A new AST case without a rule causes a "No matching case" runtime error, which the guard turns into `Incomplete`. Add the rule and a checker test in the same change as the syntax. Rules that cannot prove a construct must add a warning and return an unknown.

Match exhaustiveness uses a constructor matrix for unit, boolean, tuple, and enum types, including nested and correlated patterns. Lists support the common
empty/cons split. A column's constructors expand only when its patterns name all of them; otherwise, only rows matching anything decide coverage. Unconditional expansion can loop on recursive types: specializing a wildcard to `Node of Tree` repeats the same row. Infinite literal domains and coverage relying only on guards are conservative: if full coverage cannot be proved, the expression is
`Incomplete`.

The checker needs a declared or trusted signature to use a runtime value as evidence of its type. Builtin signatures are part of the trust boundary.
Concrete results use their actual package types. Ordinary generic builtins declare or structurally expose their type variables. Two signature cases need special handling:

- A result type variable that no parameter constrains (`Hash -> Option<'a>`,
  `optOrRes -> 'a`) means the result is only known at runtime. The checker detects this
  from the signature and treats the builtin as unsupported rather than quantifying the
  variable. No builtin is recognized by name for this.
- The polymorphic operator builtins (`add`, `lessThan`, `equals`, `negate`, ...)
  declare independent `'a`/`'b` parameters because the type language has no numeric
  constraint, but raise at runtime on anything but values of the same numeric type.
  A by-name call (`Builtin.add a b`) is checked with the operator's numeric table
  rather than the declared signature; used as a value or partially applied there is
  no signature to give them, and the use is `Incomplete`.

Infix syntax does not lower to those builtins. `a + b` is `Stdlib.Add.add a b`
(`NumericTraits.ofInfix`, likewise `- * / % **` and the four comparisons), and `-x`, which
the parser stores as `Builtin.negate x`, runs as `Stdlib.Negate.negate x`. The checker
treats each as a trait method call. `==` is not one: equality is structural, so the
operands must unify and nothing further is owed.

## Traits

A trait and an impl are package items, read off the PT (`ImplEntry.ofImpl`) the way the
runtime reads its dispatch candidates. `validateImpl` checks the method set
(`ImplMethodSet`), each method fn against the trait's signature at the self type
(`ImplMethodSignature`), and the impl fn's ceiling against the method's
(`ImplExceedsCeiling`).

- A `TraitMethod` call is typed from the trait's method, with the trait's first type
  parameter as self (`traitMethodSignature`). A bounded signature adds one constraint per
  bound on the instantiated variable; an operator adds one for its trait on the operand type.
- Constraints discharge at `finish`, after substitution. A concrete head needs exactly one
  visible impl (`MissingImpl`, `AmbiguousImpl`; a blanket `impl<'a> T for 'a` loses to a
  specific one). The item's own rigid parameter needs the bound declared on the item
  (`UnboundTypeParameter`). An inference variable still unbound is a `ConstrainedType`
  blocker, not a diagnostic.
- A conditional impl owes its own bounds at the type it matched: `Show List<Option<Int>>`
  against `impl<'a: Show> Show for List<'a>` owes `Show Option<Int>`, round by round until
  the type is exhausted (`dischargeConstraints`).
- `x.m`, where `x` is a record without a field `m`, falls back to the one visible impl
  carrying a method `m` for `x`'s head type; no such impl keeps `UnknownRecordField`.
- Visible means in `Environment.traitImpls`: every implementation bound on the branch in the
  store, plus the batch's own (`Environment.withBatch`). A trait the batch declares is read
  from the batch too (`batchTraits`), so a trait, its implementations and their callers can be
  saved in one `dark module` and pin exactly as they would saved one at a time. Without the
  batch's own, the same code pinned or not depending on how its saves were batched, and nothing
  reported it (`gates trait-choice-any-batching`).
- An authoring save that leaves any call unpinned says so, naming the items: a trait call still
  `Unknown`, or a call into a bounded fn with no recorded bounds, which includes every piped one
  (`EPipeFnCall` has no field for them).

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

./scripts/run-cli test Darklang.LanguageTools.AtRestTypeChecker
./scripts/run-cli typecheck

`packages/darklang/tests/languageTools/atRestTypeChecker.dark` tests each rule and issue code. Cases use source parsed by
`LanguageTools.Parser.Parse.packageSourceToOps` where possible, including declarations that refer to each other. Hand-built cases cover inputs the parser cannot produce: unresolved names, missing hashes, and or-alternatives binding different names. Guarded deep-input tests must either check successfully or return `Incomplete` for excessive depth, without crashing the process.

`CliScm.Tests.fs` tests rollout policy through the real CLI: definite errors are saved as WIP, rejected at commit, and accepted with `--allow-type-errors`.

The Dark test file's `Invariants` module combines interacting cases: delayed inference, incompatible operations, dictionary-key reachability, structural cycles, deep duplicate findings, and a mix of successful, failed, and missing dependency loads.

F# tests cover runtime support. `DeepValues` in `Interpreter.Tests.fs` tests value comparison and type merging; `DeepProgramTypes` in
`Serialization.DarkTypes.Tests.fs` tests outbound AST conversion. New recursive F# walks at this boundary need `RuntimeHelpers.EnsureSufficientExecutionStack()` and small-stack thread tests in both Debug and published Release. Small stacks exercise failure without inputs so large that allocation stalls the parallel suite.

A full-corpus `typecheck` is also a rollout gate. Before enabling blocking, compare representative findings with their source; failure counts alone are insufficient. Add a regression test for every false positive. Better inference can turn previously `Incomplete` items into definite failures, so new failures are not automatically regressions. Failed declarations remain saveable as WIP; incomplete ones remain committable.
