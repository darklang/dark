-- Schema for the Dark SQLite DB. ONE FILE — Migrations.fs hashes
-- this file and kill-and-fills on change. Edit freely.
--
-- Background: this is the result of concatenating the original
-- 13 incremental .sql files (chronological order). DDL is
-- intentionally not "resolved" — each `CREATE TABLE IF NOT EXISTS`
-- / `ALTER TABLE … ADD COLUMN` / `DROP TABLE … CREATE TABLE` runs
-- against an empty DB and produces the final shape. A pretty pass
-- to render only the final tables is a worthwhile follow-up; not
-- required for correctness because of the kill-and-fill semantics.

-- note that the system_migration_v0 table is also crated by Migrations.fs
CREATE TABLE IF NOT EXISTS
system_migrations_v0
( name TEXT PRIMARY KEY
, execution_date TEXT NOT NULL -- timestamp
, sql TEXT NOT NULL
);


--------------------
-- Stuff that belongs in "user space"
--------------------
-- Single-instance Dark: one DB per process, no per-scope key.
-- Test isolation is handled by wiping these tables between tests
-- (`TestUtils.initializeTestCanvas` truncates), not by scoping rows.

-- User K/V DBs
CREATE TABLE IF NOT EXISTS
user_data_v0
( id TEXT PRIMARY KEY
, table_tlid INTEGER NOT NULL
, user_version INTEGER NOT NULL
, dark_version INTEGER NOT NULL
, data TEXT NOT NULL -- JSON stored as text
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
, updated_at TEXT NOT NULL DEFAULT (datetime('now'))
, key TEXT NOT NULL
, UNIQUE (table_tlid, dark_version, user_version, key)
);

CREATE INDEX IF NOT EXISTS
idx_user_data_fetch
ON user_data_v0
(table_tlid, user_version, dark_version);

CREATE INDEX IF NOT EXISTS
idx_user_data_current_data_for_tlid
ON user_data_v0
(user_version, dark_version, table_tlid);

-- No GIN index equivalent in SQLite
CREATE INDEX IF NOT EXISTS
idx_user_data_json
ON user_data_v0
(data);


-- HTTP Handlers
-- TODO: extract out table of http handlers from toplevels_v0



-- Top-levels
-- TODO split this into a few tables (dbs, handlers, etc)
CREATE TABLE IF NOT EXISTS
toplevels_v0
( tlid INTEGER NOT NULL PRIMARY KEY
, digest CHAR(32) NOT NULL
, tipe TEXT NOT NULL CHECK (tipe IN ('db', 'handler'))
, name TEXT /* handlers only - used for http lookups */
, module TEXT /* handlers only */
, modifier TEXT /* handlers only */
, updated_at TEXT NOT NULL DEFAULT (datetime('now'))
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
, deleted INTEGER NOT NULL CHECK (deleted IN (0,1))
, data BLOB NOT NULL
);

-- Traces
CREATE TABLE IF NOT EXISTS
traces_v0
( id TEXT PRIMARY KEY
, trace_id TEXT NOT NULL -- why do we need this _and_ `id`?
-- the handler's (or for a function's default trace, the function's) TLID
--   (used to store the trace data in Cloud Storage)
-- TODO consider using a different mechanism here - fns might not have tlids...
--   why wouldn't we use the `id` instead? length?
, root_tlid INTEGER NOT NULL
, callgraph_tlids TEXT NOT NULL -- functions called during the trace
);-- Scripts
CREATE TABLE IF NOT EXISTS
scripts_v0
( id TEXT PRIMARY KEY
, name TEXT NOT NULL UNIQUE
, text TEXT NOT NULL
);-- Package storage schema
-- Content tables store definitions (content-addressed)
-- Locations table maps names to content (branch-scoped)
-- Package ops are the source of truth for all changes (branch-scoped)

-- Branches table
CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,

  -- (only) `main` has NULL here
  parent_branch_id TEXT REFERENCES branches(id),

  -- fork point on parent
  -- note: the FK is added after `commits` table is added
  base_commit_hash TEXT,

  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  merged_at TIMESTAMP -- NULL until merged
);

-- Well-known main branch
INSERT OR IGNORE INTO branches (id, name) VALUES ('89282547-e4e6-4986-bcb6-db74bc6a8c0f', 'main');


-- Commits table
CREATE TABLE IF NOT EXISTS commits (
  hash TEXT PRIMARY KEY,
  message TEXT NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_commits_branch ON commits(branch_id, created_at DESC);


-- Content tables store definitions
-- (content-addressed, no naming, no branch)
CREATE TABLE IF NOT EXISTS package_types (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS package_values (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_dval BLOB,  -- NULL until evaluated. CLEANUP once we're bootstrapped, can be tidied
  value_type BLOB, -- for finding values of a given ValueType
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_package_values_type ON package_values(value_type);

CREATE TABLE IF NOT EXISTS package_functions (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);


-- Locations table: branch-scoped name resolution
-- Maps (owner, modules, name) -> hash within a branch context
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,  -- 'fn', 'type', or 'value'
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),  -- NULL = WIP
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  deprecated_at TIMESTAMP NULL
);

CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(branch_id, owner, modules, name, item_type)
  WHERE deprecated_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE deprecated_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_wip
  ON locations(branch_id)
  WHERE commit_hash IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);

CREATE INDEX IF NOT EXISTS idx_locations_commit_hash ON locations(commit_hash);


-- Package ops: the source of truth for all package changes (branch-scoped)
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT PRIMARY KEY,
  op_blob BLOB NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),  -- NULL = WIP
  applied INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_package_ops_wip
  ON package_ops(branch_id)
  WHERE commit_hash IS NULL;

CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied ON package_ops(applied) WHERE applied = 0;
CREATE INDEX IF NOT EXISTS idx_package_ops_commit_hash ON package_ops(commit_hash);

-- Dependency tracking
CREATE TABLE IF NOT EXISTS package_dependencies (
  item_hash TEXT NOT NULL,
  depends_on_hash TEXT NOT NULL,
  PRIMARY KEY (item_hash, depends_on_hash)
);

CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on ON package_dependencies(depends_on_hash);
CREATE INDEX IF NOT EXISTS idx_package_dependencies_item ON package_dependencies(item_hash);
-- Add propagation_id column to package_ops table for direct lookup of PropagateUpdate ops
ALTER TABLE package_ops ADD COLUMN propagation_id TEXT NULL;

-- Create partial index on propagation_id for efficient lookups (only index non-null values)
CREATE INDEX IF NOT EXISTS idx_package_ops_propagation_id
  ON package_ops(propagation_id) WHERE propagation_id IS NOT NULL;
-- Add archived_at to branches (replacing hard delete)
ALTER TABLE branches ADD COLUMN archived_at TIMESTAMP NULL;

-- BranchOps table
CREATE TABLE IF NOT EXISTS branch_ops (
  id TEXT PRIMARY KEY,                -- content-addressed hash of the op
  op_blob BLOB NOT NULL,              -- serialized BranchOp
  applied INTEGER NOT NULL DEFAULT 0, -- 0=pending, 1=applied (for crash recovery)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_branch_ops_created_at ON branch_ops(created_at);
-- Add trace metadata columns to traces_v0
ALTER TABLE traces_v0 ADD COLUMN handler_desc TEXT NOT NULL DEFAULT '';
ALTER TABLE traces_v0 ADD COLUMN timestamp TEXT NOT NULL DEFAULT '';


CREATE TABLE IF NOT EXISTS trace_inputs_v0
( trace_id TEXT NOT NULL
, name TEXT NOT NULL
, value_json TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS trace_fn_results_v0
( trace_id TEXT NOT NULL
, fn_name TEXT NOT NULL
, args_hash TEXT NOT NULL
, hash_version INTEGER NOT NULL DEFAULT 0
, result_json TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS trace_fn_arguments_v0
( trace_id TEXT NOT NULL
, fn_name TEXT NOT NULL
, args_hash TEXT NOT NULL
, args_json TEXT NOT NULL DEFAULT '[]'
);

CREATE INDEX IF NOT EXISTS idx_trace_inputs_trace_id ON trace_inputs_v0(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_results_trace_id ON trace_fn_results_v0(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_results_fn_name ON trace_fn_results_v0(fn_name);
CREATE INDEX IF NOT EXISTS idx_trace_fn_results_lookup
  ON trace_fn_results_v0(trace_id, fn_name, args_hash);
CREATE INDEX IF NOT EXISTS idx_trace_fn_arguments_trace_id ON trace_fn_arguments_v0(trace_id);
-- Deprecation redesign schema changes.
--
-- 1. Rename locations.deprecated_at to unlisted_at.
--    The previous name was misleading: the column tracks pointer lifecycle
--    (renames, propagation, WIP->committed swaps) rather than author-initiated
--    deprecation. Freeing the word "deprecated" for the new deprecation-as-op
--    annotation layer.
--
--    Done via table swap. SQLite's ALTER TABLE RENAME COLUMN trips on the
--    original partial indexes (WHERE deprecated_at IS NULL) during the
--    multi-statement transaction — dropping them up front wasn't enough to
--    work around the sqlite_schema validation pass, so we rebuild instead.
--
-- 2. Add the deprecations table.
--    Branch-scoped projection of Deprecate/Undeprecate ops. Shape mirrors
--    locations (branch-scoped, commit-or-WIP, superseded via unlisted_at) so
--    the branch-chain visibility pattern reuses the same JOIN shape the rest
--    of the PM already uses.

CREATE TABLE locations_new (
  location_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,  -- 'fn', 'type', or 'value'
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),  -- NULL = WIP
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP NULL
);

INSERT INTO locations_new
  (location_id, item_hash, owner, modules, name, item_type,
   branch_id, commit_hash, created_at, unlisted_at)
SELECT location_id, item_hash, owner, modules, name, item_type,
       branch_id, commit_hash, created_at, deprecated_at
FROM locations;

DROP TABLE locations;
ALTER TABLE locations_new RENAME TO locations;

CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(branch_id, owner, modules, name, item_type)
  WHERE unlisted_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE unlisted_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_wip
  ON locations(branch_id) WHERE commit_hash IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);

CREATE INDEX IF NOT EXISTS idx_locations_commit_hash ON locations(commit_hash);


CREATE TABLE IF NOT EXISTS deprecations (
  deprecation_id    TEXT PRIMARY KEY,
  branch_id         TEXT NOT NULL REFERENCES branches(id),
  commit_hash       TEXT REFERENCES commits(hash),   -- NULL = WIP
  item_hash         TEXT NOT NULL,
  item_kind         TEXT NOT NULL,                   -- 'fn' | 'type' | 'value'

  -- 'deprecated' (annotation_blob has kind + message + optional replacement ref)
  -- 'undeprecated' (annotation_blob NULL) — used for ancestor-override on child branches
  state             TEXT NOT NULL,
  annotation_blob   BLOB,

  created_at        TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at       TIMESTAMP                        -- set when a later row supersedes this one
);

CREATE INDEX IF NOT EXISTS idx_deprecations_lookup
  ON deprecations(branch_id, item_hash, item_kind) WHERE unlisted_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_deprecations_wip
  ON deprecations(branch_id) WHERE commit_hash IS NULL;

CREATE INDEX IF NOT EXISTS idx_deprecations_commit_hash
  ON deprecations(commit_hash);
-- Merge trace_fn_results_v0 + trace_fn_arguments_v0 (from 20260318_000000)
-- into one table.
--
-- The split was shaped like "args table + results table joined by
-- args_hash," but the two tables were 1:1 in cardinality and keyed the
-- same way. args_hash was written and indexed but never used as a dedup
-- key: no write path checked it before inserting, no read path queried
-- by it. Every read was a LEFT JOIN reassembling what a single row
-- would have held. One row per fn call is what the viewer actually
-- consumes.


DROP INDEX IF EXISTS idx_trace_fn_results_trace_id;
DROP INDEX IF EXISTS idx_trace_fn_results_fn_name;
DROP INDEX IF EXISTS idx_trace_fn_results_lookup;
DROP INDEX IF EXISTS idx_trace_fn_arguments_trace_id;
DROP TABLE IF EXISTS trace_fn_arguments_v0;
DROP TABLE IF EXISTS trace_fn_results_v0;

CREATE TABLE trace_fn_calls
( trace_id TEXT NOT NULL
, fn_name TEXT NOT NULL
, args_json TEXT NOT NULL
, result_json TEXT NOT NULL
);

CREATE INDEX idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX idx_trace_fn_calls_fn_name ON trace_fn_calls(fn_name);
-- Content-addressed blob storage.
-- Bytes keyed by SHA-256 hash; dedup comes for free via PK uniqueness.
-- Orphan rows are reclaimed by `LibDB.RuntimeTypes.Blob.sweepOrphans`.
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
-- Redesign trace_fn_calls: every fn call AND every lambda invocation becomes
-- a row, linked via parent_call_id. Drop the old _v0 trace tables.
--
-- Also drop traces_v0's redundant `trace_id` (= `id`) and unused
-- `callgraph_tlids` (always written as ''). Rename traces_v0 → traces
-- to match the post-Oct-2025 naming convention. Inline the handler input
-- onto the trace row (it's always one var per trace).

DROP INDEX IF EXISTS idx_trace_fn_calls_trace_id;
DROP INDEX IF EXISTS idx_trace_fn_calls_fn_name;
DROP INDEX IF EXISTS idx_trace_inputs_trace_id;
DROP TABLE IF EXISTS trace_fn_calls;
DROP TABLE IF EXISTS trace_inputs_v0;
DROP TABLE IF EXISTS traces_v0;


-- One row per handler invocation. The handler input (parsed dval bound to
-- the handler's parameter — `request` for HTTP, `expression` for eval) lives
-- directly here as JSON, not in a separate one-row-per-trace table.
CREATE TABLE traces
( id               TEXT PRIMARY KEY
, root_tlid        INTEGER NOT NULL
, handler_desc     TEXT NOT NULL
, timestamp        TEXT NOT NULL
, input_name       TEXT NOT NULL
, input_value_json TEXT NOT NULL
);


-- Every fn call and every lambda invocation gets one row. parent_call_id
-- points directly at the calling event's call_id (NULL for source-level).
-- kind discriminates fn / lambda / builtin so the renderer can pick the
-- right tag without inspecting fn_hash.
CREATE TABLE trace_fn_calls
( trace_id        TEXT NOT NULL
, call_id         TEXT NOT NULL
, parent_call_id  TEXT                            -- NULL for source-level
, kind            TEXT NOT NULL                   -- "function" | "lambda" | "builtin"
, fn_hash         TEXT                            -- callee for function/builtin
, lambda_expr_id  TEXT                            -- AST id of the lambda body
, args_json       TEXT NOT NULL                   -- JSON array of dval JSONs
, result_json     TEXT NOT NULL
, PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);
-- Add per-call timing to trace_fn_calls. NOT NULL with default 0 so old
-- rows backfill cheaply. Function and lambda frames get real durations
-- (frame-entry to frame-exit); builtins remain at 0 since the recorder
-- only sees their synchronous storeFnResult, with no matching entry hook.

ALTER TABLE trace_fn_calls ADD COLUMN duration_ms INTEGER NOT NULL DEFAULT 0;
-- Rebuild `package_dependencies` to record the user-typed FQN on each
-- dep edge alongside the content hash. The original
-- `(item_hash, depends_on_hash)` PK silently dropped the second edge
-- when one item referenced two distinct FQNs that shared a hash
-- (e.g. multiple `val ... = 200L`) — INSERT OR IGNORE rejected the
-- duplicate even though the location differed.
--
-- The UNIQUE INDEX wraps the location columns in `COALESCE(col, '')`
-- because SQLite treats raw NULLs as distinct in a UNIQUE index;
-- without coalescing, duplicate NULL-location rows would accumulate
-- on every `updateDependencies` reinsert.
--
-- `package_dependencies` is fully derived from package items, so we
-- drop and recreate; no data preservation needed.

DROP TABLE IF EXISTS package_dependencies;

CREATE TABLE package_dependencies (
  item_hash TEXT NOT NULL,
  depends_on_hash TEXT NOT NULL,
  depends_on_item_type TEXT NOT NULL,
  depends_on_owner TEXT,
  depends_on_modules TEXT,
  depends_on_name TEXT
);

CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on
  ON package_dependencies(depends_on_hash);
CREATE INDEX IF NOT EXISTS idx_package_dependencies_item
  ON package_dependencies(item_hash);

-- Partial index for the propagation query: "who depends on this
-- location?" Excludes the NULL backlog so it stays small.
CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on_location
  ON package_dependencies(depends_on_item_type, depends_on_owner, depends_on_modules, depends_on_name)
  WHERE depends_on_owner IS NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS idx_package_dependencies_unique
  ON package_dependencies(
    item_hash,
    depends_on_hash,
    depends_on_item_type,
    COALESCE(depends_on_owner, ''),
    COALESCE(depends_on_modules, ''),
    COALESCE(depends_on_name, '')
  );
-- Restore the `accounts_v0` table + commits/traces account_id columns
-- that were dropped in this branch's account-removal pass. Coworker
-- re-flagged: accounts (the developer behind a commit) belong;
-- canvas / app / domain were the right thing to drop, accounts
-- weren't.
--
-- Single-instance Dark today means there's effectively one account
-- per process, but tracking who authored a commit is still load-
-- bearing for audit + the future multi-tenant story. Schema-level
-- accounts are cheap and cleanly separated from canvas/app/domain
-- (which stays gone).

CREATE TABLE IF NOT EXISTS accounts_v0
( id TEXT PRIMARY KEY
, name TEXT NOT NULL UNIQUE
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Seed: pre-allocated UUIDs so Dark code can reference them by ID.
-- These IDs are part of the API; don't rotate.
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES
  ('00000000-0000-0000-0000-000000000001', 'Darklang'),
  ('00000000-0000-0000-0000-000000000002', 'Stachu'),
  ('00000000-0000-0000-0000-000000000003', 'Paul'),
  ('00000000-0000-0000-0000-000000000004', 'Feriel');

-- Commits: who authored. Default to Darklang so the migration is
-- safe on existing rows (CLI uses Darklang as the default account).
ALTER TABLE commits
  ADD COLUMN account_id TEXT NOT NULL
  DEFAULT '00000000-0000-0000-0000-000000000001'
  REFERENCES accounts_v0(id);

-- Traces: who triggered. Nullable — existing trace rows pre-date
-- account attribution. New rows always carry the account.
ALTER TABLE traces ADD COLUMN account_id TEXT REFERENCES accounts_v0(id);
