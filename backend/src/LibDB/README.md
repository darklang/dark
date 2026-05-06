# LibDB

SQLite-backed persistence for the package manager, branches, SCM ops,
user DBs, and traces. Companion to the in-memory `PT.PackageManager`
in `LibExecution`.

Surface (most relevant first):

- `PackageManager.fs` — `pt(branchId)` builds a branch-scoped PM that
  resolves names against the branch's chain.
- `Branches.fs` / `Inserts.fs` / `Rebase.fs` / `Merge.fs` — SCM ops
  (CRUD, conflict detection, rebase, merge into parent).
- `Queries.fs` — branch-aware SQL for resolving package items.
- `Caching.fs` — in-process cache for repeated lookups.
- `UserDB.fs` — runtime CRUD for user-defined Datastores
  (`Stdlib.DB.set` / `get` / etc.). Per-row isolation is keyed by
  `program.scopeID` (a UUID).
- `Tracing.fs` — per-execution trace recorder. Writes the trace row +
  fn_call rows to the SQLite DB; `Builtins.Tracing/Libs/Traces.fs`
  reads them back.
- `Seed.fs` — bootstraps the DB from the embedded seed when missing.

Items are content-addressed (SHA256-keyed); locations (name → hash
bindings) are branch-scoped.
