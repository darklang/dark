# Internals worth knowing before you change them

The store's schema, SCM and branches, traits, and where text measurement lives. Each section is
the shape of the code and the rule that keeps it correct; the traps that are not specific to one
of these areas are in `docs/agent-traps.md`.

## Changing the schema

**`migrations/schema/00-initial-schema.sql` is ONE file, edited in place.** It declares the shape a
FRESH store is born with. It used to be fourteen numbered subsystem files, each frozen once merged, so
that a filename meant the same thing in every store ever made from it. That rule was dropped when the
molt made every pre-existing store unopenable anyway: a store older than the cut is not supported, and
`dark` tells you to delete it and start again rather than pretending to upgrade it.

So:

- A NEW TABLE or INDEX goes in a stamped file beside it (`scripts/migrations/new` makes one), or
  straight into `00-initial-schema.sql` while the PR is unmerged. Filename order is still the apply
  order, and a FK target must be declared before the table that references it.
- A NEW COLUMN takes BOTH halves, and this is the one to get right:
  - declare it on the table in `00-initial-schema.sql`, for fresh stores;
  - carry it to existing stores with a step in `LibDB/Releases.fs`.
  A schema file only ever runs `CREATE TABLE` / `CREATE INDEX` / `INSERT OR IGNORE`
  (`Releases.applySchemaTables`), so it cannot `ALTER`, and a step is the only thing that can.

**Skipping the first half is invisible until somebody collapses the schema.** `addColumnIfMissing`
runs on a FRESH store too -- the table exists, the column does not, so it adds it -- which means a
column declared ONLY in a step still appears in a new store, and nothing ever says the schema file is
incomplete. Twelve columns on `traces` and `trace_fn_calls` were in exactly that state, and the only
thing that surfaced them was deleting the steps and watching a fresh store fail on `no such column:
status`. If you add a column, grep the schema file for it before you call it done.

Adding a file changes the schema hash, which drops and re-folds the projection tables. That is cheap and
expected; canonical tables are never dropped, which is the reason a column needs the `Releases` step.

## SCM and branches

`package_ops` is canonical: one row per op per branch, and an op's id IS its content hash. Everything
else -- `locations`, `deprecations`, `location_docs`, `package_functions`, `package_dependencies` -- is a
projection you can drop and re-fold from the log. That is why a schema change to a projection costs
nothing and a change to a canonical table needs `LibDB/Releases.fs`.

Every write to the SCM tables is an ACTION in F#: commit, discard, the branch lifecycle (start, rename,
switch, archive), merge, rebase, and settling a conflict, one builtin each (`scmCommit`, `scmBranch`,
`scmMerge`, `scmDiscard`, `scmResolve`), each run in one transaction wherever the work allows. Dark keeps
the CLI and every sentence a person reads, the checks in front of an action (type check, unresolved names,
constraints, what `--include` pulls in), the status picture, docs and lineage. There is no generic write
path from Dark into the store; the reads are typed builtins too (`scmOps`, `scmBindings`, ...).

    packages/darklang/scm/     # what to make of the store, and the names callers use
      actions.dark             #   the action builtins' one wrapper each, and their types
      packageOps.dark          #   package_ops: the log
      bindings.dark            #   locations: what a branch binds, asked of that branch
      branches.dark            #   branches, branch_name_bases; canMerge lives here
      commits.dark             #   commits
      conflicts.dark           #   conflicts: the shapes, the reads, how one is shown
      constraints.dark         #   standing findings (outdated usages)
      propagation.dark         #   propagation_policy: who follows what moved
      draft.dark               #   one answer to "what have I changed"
      storeHealth.dark         #   what can be wrong with the STORE

    LibExecution/Lww.fs        # THE last-writer-wins rule. One place, on purpose; see below
    LibDB/Layers.fs            # what a branch SEES: the layered projections. Read this first.
    LibDB/PackageOpPlayback.fs # THE FOLD: ops -> projections, each op into its branch's layer
    LibDB/Inserts.fs           # author onto any branch: restate, insert, fold, record name bases
    LibDB/ScmActions.fs        # the actions: commit, discard, branch lifecycle, merge, rebase, resolve
    LibDB/ScmDetect.fs         # conflict detection, beside the rule it names winners by
    LibDB/Branches.fs          # the branch registry: ids, names, parents, lookups
    LibDB/Propagation.fs       # the cascade: who depends on what moved
    LibDB/Releases.fs          # shape changes to canonical tables on existing stores

**Last-writer-wins lives in `LibExecution/Lww.fs`, and asking it twice is the bug.** Two different things need
the rule: the fold decides which binding survives, and conflict detection decides which side to NAME as the
winner (`LibDB/ScmDetect.fs`). If those disagree, a recorded conflict names a winner the fold did not pick
and two instances converge on different content with nothing to say so. Both call the one copy, and so
does Dark (`SCM.Conflicts.beats`, through `scmLwwBeats`) rather than keeping one of its own.
`Tests/Lww.Tests.fs` pins the rule, the Dark name and the detector to the same table rows;
`packages/darklang/tests/scm/lww.dark` states the rows again from the Dark side.

**An action stopped partway leaves nothing half-done.** Each runs in one transaction; where that is not
possible (a rebase's repoints, which are propagations; settling a conflict, which authors an op through
the ordinary path) the step that cannot join comes first and is complete on its own, and the step after
it is idempotent, so running the action again finishes it. `ScmActions.Testing.failAt` stops an action at
a named step inside its transaction, and `BranchOverlay.Tests` checks the store reads as it did before.

**A branch is layers, not a copy.** Its ops are the `package_ops` rows under
its id. Names, deprecations and docs are kept per LAYER -- a branch, and whether the op that wrote the row
is committed -- and `live = 1` marks the newest row of a layer for its key. A branch SEES its draft layer,
its committed layer, then each ancestor's committed layer up to main's, nearest first (`Layers.cte`,
`Layers.visible`); main sees its draft over its committed work. So a branch never sees a parent's draft.
Commit moves rows from the draft layer to the committed one, discard deletes draft rows, and merge moves
the branch's ops to the parent and folds them there. Every read takes the branch it asks about.

**A store's own log can hold ops it cannot read.** Written when ops arrived over a wire from a
peer on a different build, which no longer happens in this tree; it still holds for any op this
binary's deserializer rejects, including one an older build of this binary wrote. Such ops are
STORED and left unapplied deliberately, so a later build can apply them, which means they sit in
the local log where every local reader meets them. So "the LOCAL log is ours, raise if it will not
parse" is false. A reader that raises dies on the first such op and stays dead (once seen as a raw
`BinaryFormatException` out of a propagation command); a writer that deletes main's log and
re-inserts what it read destroys them. The rule: ONE decoder, it returns an Option, every reader
tolerates, and no writer deletes what it could not decode (the draft rewrite deletes only the ids it
read as ops).

**A build eats ops that are not in `packages/`.** `LocalExec` calls `LibDB.Purge.purge ()` before a
package reload: it wipes the store and re-authors every item from the `.dark` files on disk, so
anything you authored by hand is gone. Measured once at 28,372 ops before a build and 12,692 after;
the absolute numbers depend on the tree and are smaller now, the halving is the point. `LocalExec`
is dev tooling, so this is a dev-clone fact rather than a product one, and we are deliberately not
fixing it.

**Authoring looks like the same bug and is not.** `WipRefresh.refresh` runs on every author on main, and
it deletes main's draft and re-inserts it re-resolved. It deletes exactly the ops it read
(`Inserts.rewriteDraftIfUnchanged`), and only if the draft is still what it read, so another author's op
and an op this build cannot decode both survive it.

## Traits

A trait is a package item (`PT.Trait`: type params, bounds, method signatures with optional
ceilings); an impl is a package item (`PT.TraitImpl`: the trait, its type args, the self
type, own params and bounds, methods as `(name, fn)` pairs). Two ops, `AddTrait` and
`AddTraitImpl`; two tables, `package_traits` and `package_trait_impls(trait_hash)`;
`FQTraitName` for references; binary format v3 (`BaseFormat.fs` lists every version since).
The parser lowers `trait`/`impl` in `SourceFile.items` (F#) and `implWithOps` (Dark); both
must agree on the impl's member path `<module>[.<Type>].<Trait>`, with the method fns as
ordinary fns beneath it. Dispatch candidates come off the index
(`PT2RT.ImplCandidate.ofPackageManager`), selection is `Traits.fs`, the checker validates
traits and impls in `AtRestTypeChecker` (`ImplMethodSet`, `ImplMethodSignature`;
`ImplExceedsCeiling` is declared and not checked yet). The operators are the stdlib traits
(`stdlib/traits.dark`); `+` lowers to `Stdlib.Add.add` through `NumericTraits.fs`, whose
hashes come from `PackageRefs.Trait`, so a new operator trait needs a ref and a regenerated
`package-ref-hashes.txt`. `==` and `!=` are NOT traits: equality is structural for every
value and lowers to its builtin. A type cannot override it, which is what keeps
`List.contains`, `List.unique`, `List.sort` and dict keys meaning the same thing as `==` --
dict keys have to be structural anyway, since `DictMap` orders them inside the runtime,
where nothing can call the interpreter.
`Zero.zero`/`One.one` dispatch from an explicit type arg or the caller's bound, so a call to
the impl fn clears the trait's type args first.

**A trait call stores the implementation it resolved to.** `FQFnName.TraitMethod` carries
`implFn`, the fn the chosen implementation names for that method, as a located reference;
`EInfix` carries the same for an operator. It is written by `resolveTraitCalls`
(`Builtins.Matter/Libs/PM/TraitCalls.fs`), which runs inside `addAuthored` BEFORE
`stabilizeHashes`, since the choice is part of what the item is: the at-rest checker says which
implementations apply at each call node (`Proof.resolutions`, keyed by the node the name is at),
and the store says which is newer. It is hashed with the item and becomes an ordinary fn
dependency edge, so propagation, `pin` and `follow` treat a newer implementation like any other
update.

**A choice that cannot be made yet says so, rather than saying nothing.** The field is an
`ImplChoice`, three states, not an option: `Chosen` (this fn), `FromTypeParam "a"` (my caller
decides, and here is which of my type params it turns on), `Unknown` (nobody could work it out;
resolve at run time). A call inside a bounded generic is the middle one: the checker knows the
self type is the item's own rigid type param, so the callee records the param name and the CALL
records what that param implied. That second half is `EFnName`'s third field, a list of
`BoundImpl` (`param`, `trait_`, `method_`, `choice`), one entry per method of the trait so the
callee's body finds a fn without reading the implementation item. `ApplicableNamedFn.boundImpls`
carries it into the runtime and `CallFrame.boundImpls` is what the body reads; a `FromTypeParam`
in a caller's own list is resolved against the caller's frame on the way in, which is what
threads a bound through a generic that calls another generic. So the only resolutions left at
run time are the genuinely unknowable ones, and a Dval's.

`gates trait-choice-survives-rival` is the property: author a bounded generic and a caller, then
author a NEWER rival implementation for the same type. The saved call keeps its answer while a
fresh call takes the new one. Break either half of the mechanism and it fails.

One trap when writing this kind of test by hand: **a type is content-addressed, so a same-shaped
record anywhere in the store IS your type.** A throwaway `Pt = { x: Int64 }` shares its hash with
every other `{ x: Int64 }` somebody left in the dev store, implementations and all, and your test
then measures their choices. Give the field a name nobody else would use.

A drop site worth knowing about, because it looks like nothing: **anything that rebuilds an
`EFnName` must carry the third field.** Drop it and the item re-hashes without its
bounds, un-deciding what the checker decided; `DeferredResolver.reResolveExpr`, which every
authoring through `WipRefresh` goes down, is the one to watch. The parser's own `EFnName` sites are the exception: they write `[]`
because the checker has not run yet.

**The disk-load path records choices too, and it is the reason a reload rehashes so much.**
`resolveTraitCalls` runs from `addAuthored` (so `dark fn`/`impl`/`module` and the workbench)
and from `LocalExec.reloadPackages`, which authors `packages/` off disk. The reload path
does it in the same order the authoring path does, records the choices at the hashes in hand
and then `computeRealHashes` to move them, and it prints how many ops moved. Recording a choice changes
an item's content hash and every caller's hash moves with it, so the op count it reports is
larger than the number of calls resolved. Two things follow: the shipped tree and `seed.db` carry
their choices, so a dispatch-cost measurement against them is measuring the resolved path; and
"a saved call goes on meaning what it meant" holds for the tree, not just for what somebody
typed at the CLI.

**`EPipeFnCall` and friends exist on BOTH `WrittenTypes.PipeExpr` and `ProgramTypes.PipeExpr`,
with different arities, and only the enclosing signature tells you which one a match is over.**
Widening the PT case and then updating every `EPipeFnCall` pattern by name broke three matches
that were over the WT type, with no build-time signal: one surfaced as a raise the authoring
guard converted into "not found", and two waited for somebody to hover or to look at syntax
highlighting. Check `(pe: WrittenTypes.PipeExpr)` against
`(p: LanguageTools.ProgramTypes.PipeExpr)` at the top of each function before touching its arms.

**Every switch over item kinds has five arms.** Types, values, fns, traits, impls.
A new listing, codec, or CLI command that handles three of them silently drops the
other two; `ls`, `tree`, `search`, completion and the workbench all handle all
five, and the names-only search builtins return six lists.
`ProgramTypes.ItemKind.toString`/`fromString` is the one spelling of the kinds as
strings, on both sides of the line (`PT.ItemKind` in F#, `LanguageTools.ProgramTypes`
in Dark); they write the same `item_type` column, so change one and change both.
`PrettyPrinter.ProgramTypes.sourceOf` is the one "render the item at this hash of
this kind", for the same reason.

**An implementation and the module its methods live in share a name, so a listing
that dedupes by name can drop the implementation.** The methods of `impl Add for
Int64` are fns under `Stdlib.Int64.Add`, which also makes `Add` look like a submodule
of `Stdlib.Int64`. A reader wants the implementation row, not the folder, so every
listing filters the folder out through `LanguageTools.PackageManager.Search.readerSubmodules`
and orders its sections by `Cli.Glyphs.kindOrder`. Five surfaces decided this for
themselves at one point or another, and the one that forgot showed seventeen empty
folders and no implementations. It lives in the package manager rather than in the CLI
because every reader needs it.

**A bare trait name falls back to the stdlib.** `impl Add for Point` in any module
means `Stdlib.Add` unless something closer is called Add (both resolvers,
`resolveTraitName` and `TraitName.resolve`). Two same-shaped traits still hash the
same; a trait and a same-shaped record do not.

**An impl over fns that already exist is an alias block.** `impl Add for String = let
add = Stdlib.String.append` generates no fn; the impl names the existing one. The
interpreter answers two operands of one builtin numeric type without dispatch
(`FastOps.evalNumeric`), so those aliases are what the checker and `dark impls` see,
not what runs.

**An impl is a candidate only while a name binds it on the branch asked and it is not
deprecated** (`ImplCandidate` liveness, then `Queries.getDeprecatedTraitImplHashes` in the
store's candidate provider). Two live implementations of one trait for one type do not error:
the one whose `AddTraitImpl` op is newer runs. The stamp rides on `package_trait_impls.origin_ts`,
the candidate carries it, and `Traits.select` asks `LibExecution.Lww` -- the same rule the op-fold
uses for two bindings of one name, so this must not grow a second copy. The pair is the
`rival-implementations` finding in `dark constraints` (Dark computes the same winner through
`SCM.Conflicts.beats`), and deprecating one settles it. A call errors only when NO rival carries a
stamp, which means nothing came from an op. Dispatch is memoised on the package manager
(`implSelectionMemo`, keyed by branch, trait, method and self type) under
`LibDB.Caching.generation`, which every `invalidateAll` bumps; a side-loaded manager
(`withExtraImpls`, `withExtras`) gets its own memo, and the script host's child state
must take the grafted manager's `getTrait` too, or a script's own trait is "not in the
package manager" at dispatch.

## Measuring text, and what may go native

Pretty-printing lives in Dark; `Execution.fs` looks the printers up through `PackageRefs` and
calls them. **F# owns measurement, Dark owns layout.** How wide a character is, is a table lookup
(`Prelude/TextWidth.fs`). Where a line breaks is not.

The trap: a Dark loop pays a builtin call per character. `Stdlib.Pretty` measures once per `Doc`
node and is fine in Dark; the row fitters in `Builtins.Cli/Libs/TerminalText.fs` walked characters
and cost seconds per keystroke, which is why they're native.

"Characters bad, spans fine" is the wrong test, though. `Canvas.compose` only ever touches spans, a
couple of hundred per frame, and is still the largest thing in a view build -- because it runs about a
dozen interpreted operations on each one.

So count **operations per frame**, not what they operate on. A Dark loop over 250 items doing a dozen
calls each is 3,000 operations, and that is the number that decides whether something belongs in F#.

Widths are display cells, never `String.length`. `Stdlib.String.displayWidth` measures them and is
medium-independent; `Cli.Tui.Text` is only for rows that carry escape sequences.
`Stdlib.Text` aligns plain text in columns using screen-cell widths. `padEndToWidth` and
`padStartToWidth` add padding to reach a given width; `clipToWidth` and `ellipsis` shorten text to
fit. By comparison, `String.padEnd` and `String.padStart` count characters, so wide glyphs can
throw columns out of alignment.
