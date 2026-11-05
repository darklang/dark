# LibDB

SQLite-backed persistence for the package store, branches, the op log, and traces. Companion to the in-memory `PT.PackageManager` in `LibExecution`.

The model: `package_ops` is the source of truth, one row per op per branch, and everything else here is a projection folded from it. Items are content-addressed (SHA256-keyed) and shared by every branch. Names, deprecations and docs are LAYERED (`Layers.fs`): each row belongs to a branch and to its draft or its committed work, and a branch sees its own layers over its ancestors' committed ones. Every read names the branch it asks about.

Surface (most relevant first):

- `Layers.fs`: what a branch SEES, and the row bookkeeping the layered tables share (supersede, move to committed, drop).
- `PackageOpPlayback.fs`: the fold. Applies each `PackageOp` to the projection tables, each into its branch's layer, and is where timestamp-LWW decides which of two bindings for a name wins.
- `Inserts.fs`: the write path for every branch: restate, insert, fold, record name bases, in one transaction. `rewriteDraftIfUnchanged` is the draft refresh's delete-and-reinsert; it writes only if the draft is still what its caller read.
- `Branches.fs`: the branch registry: ids, names, parents, lookups.
- `BranchSelection.fs`: which branch a process runs on, in order: `--branch`, then `DARK_BRANCH`, then the stored `current_branch`.
- `Queries.fs`: reads over the log and the projections. The draft, a commit's ops, dependents and deprecations.
- `ProgramTypes.fs`: the SQL behind name resolution and `search`, over a branch's view of `locations`.
- `PackageManager.fs`: the `PT` and `RT` `PackageManager` records, one PT manager per branch, plus `withExtraOps` for ops that are not stored (parsing, tests).
- `Propagation.fs`: the cascade. Which dependents follow a moved item, and the ops that repoint them.
- `WipRefresh.fs`: re-resolving and rewriting main's draft.
- `Seed.fs`: seed export, and the startup grow that folds pending ops and evaluates values.
- `Releases.fs`: shape changes to canonical tables on stores that already exist, which `CREATE TABLE IF NOT EXISTS` cannot carry.
- `Caching.fs`: in-process caches over the store, and the one call that drops them all when ops fold.
- `Tracing.fs`: the per-execution trace recorder. Writes the trace row plus fn_call rows; `Builtins.Matter/Libs/Traces.fs` reads them back.

The SCM actions (commit, discard, merge, rebase, archive, resolution) are `ScmActions.fs`, one transaction each; Dark under `packages/darklang/scm/` keeps the CLI, the checks in front of an action, and every sentence a person reads, and reads the store through typed builtins (`Scm*.fs`).
