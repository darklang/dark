# LibDB

SQLite-backed persistence for the package store, branches, the op log, user DBs, and traces. Companion to the in-memory `PT.PackageManager` in `LibExecution`.

The model: `package_ops` is the source of truth, and everything else here is a projection folded from it. Items are content-addressed (SHA256-keyed); `locations` maps names to hashes and holds MAIN's bindings only. A branch is not a copy of anything: its ops sit in the same log at `effective = 0`, tagged in `op_branches`, and its package manager is main's with those ops overlaid.

Surface (most relevant first):

- `PackageOpPlayback.fs`: the fold. Applies each `PackageOp` to the projection tables, and is where timestamp-LWW decides which of two bindings for a name wins.
- `Inserts.fs`: the write path, which inserts ops, folds them, then marks them applied. `rewriteOpsAtomically` is the delete-and-reinsert one, used by the draft rewrite.
- `Branches.fs`: the branch store. Registry rows, the `op_branches` frontier, per-name bases, and the chain walk that overlays a branch on its parents.
- `BranchSelection.fs`: which branch a process runs on, in order: `--branch`, then `DARK_BRANCH`, then the stored `current_branch`.
- `Queries.fs`: reads over the log and the projections. The draft, a commit's ops, dependents, deprecations, propagation policy.
- `ProgramTypes.fs`: the SQL behind name resolution and `search`, over `locations`.
- `PackageManager.fs`: the `PT` and `RT` `PackageManager` records, plus `withExtraOps` and the branch overlays built from it.
- `Propagation.fs`: the cascade. Which dependents follow a moved item, and the ops that repoint them.
- `WipRefresh.fs` / `Draft.fs`: re-resolving and rewriting main's draft.
- `Seed.fs`: seed export, and the startup grow that folds pending ops and evaluates values.
- `Releases.fs`: shape changes to canonical tables on stores that already exist, which `CREATE TABLE IF NOT EXISTS` cannot carry.
- `Caching.fs`: in-process caches over the store, and the one call that drops them all when ops fold.
- `UserDB.fs`: runtime CRUD for user-defined Datastores (`Stdlib.DB.set` / `get` / etc.). Rows are global, keyed by `(table_tlid, key)`.
- `Tracing.fs`: the per-execution trace recorder. Writes the trace row plus fn_call rows; `Builtins.Matter/Libs/Traces.fs` reads them back.

The SCM verbs themselves (merge, rebase, conflict presentation, resolution) live in Dark, under `packages/darklang/scm/`. What stays here is what Dark cannot do: hashing, serialization, and the SQL that has to run mid-authoring.
