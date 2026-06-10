-- Schema for the Dark SQLite DB. ONE FILE — Migrations.fs hashes
-- this file and kill-and-fills on change. Edit freely.
--
-- Each table is defined ONCE in its final form. No "create then ALTER",
-- no "build vN then DROP and rebuild as vN+1" — kill-and-fill means the
-- final shape is what runs against an empty DB.
--
-- system_migrations_v0 (the legacy per-named-migration table) is the one
-- exception, since legacy DBs are adopted via that table; created
-- here AND by Migrations.fs's adoptLegacyDB path.
--
-- Order: bookkeeping → branches → commits → ops → package projections →
-- locations → traces → user-data, toplevels, scripts. FK targets come
-- before FK sources so kill-and-fill replays cleanly.


--------------------
-- Bookkeeping
--------------------

CREATE TABLE IF NOT EXISTS system_migrations_v0 (
  name TEXT PRIMARY KEY,
  execution_date TEXT NOT NULL,  -- ISO-8601 timestamp
  sql TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS accounts_v0 (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Seed: pre-allocated UUIDs so Dark code can reference accounts by ID.
-- These IDs are part of the API; don't rotate. Single-instance Dark
-- today means effectively one account per process, but tracking who
-- authored a commit is load-bearing for audit + the future
-- multi-tenant story.
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES
  ('00000000-0000-0000-0000-000000000001', 'Darklang'),
  ('00000000-0000-0000-0000-000000000002', 'Stachu'),
  ('00000000-0000-0000-0000-000000000003', 'Paul'),
  ('00000000-0000-0000-0000-000000000004', 'Feriel');


--------------------
-- Branches, commits, ops
--------------------

CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,

  -- (only) `main` has NULL here
  parent_branch_id TEXT REFERENCES branches(id),

  -- fork point on parent. Self-referential FK to commits is cyclic
  -- (commits also references branches), but the rows are inserted in
  -- dependency order at runtime; we don't add a FK constraint here.
  base_commit_hash TEXT,

  archived_at TIMESTAMP NULL,             -- soft-delete; replaces hard delete
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  merged_at TIMESTAMP                     -- NULL until merged
);

-- Well-known main branch
INSERT OR IGNORE INTO branches (id, name)
  VALUES ('89282547-e4e6-4986-bcb6-db74bc6a8c0f', 'main');


CREATE TABLE IF NOT EXISTS commits (
  hash TEXT PRIMARY KEY,
  message TEXT NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),

  -- Default to Darklang so existing rows have a safe attribution if
  -- the column is added on a pre-existing DB; new rows from
  -- `dark commit` always pass the active account explicitly.
  account_id TEXT NOT NULL
    DEFAULT '00000000-0000-0000-0000-000000000001'
    REFERENCES accounts_v0(id),

  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_commits_branch
  ON commits(branch_id, created_at DESC);


-- The source of truth for all package changes (branch-scoped).
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT PRIMARY KEY,
  op_blob BLOB NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),  -- NULL = WIP
  applied INTEGER NOT NULL DEFAULT 0,
  propagation_id TEXT NULL,                   -- direct lookup for PropagateUpdate ops
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  -- Authoring timestamp, PORTABLE across sync. A
  -- locally-authored op self-stamps here at insert; a SYNCED op preserves its origin (the sync
  -- receiver writes the peer's value), so every instance agrees on a given op's origin_ts and
  -- max(origin_ts) picks the same divergence winner → no swap. Distinct from `created_at` (which
  -- is local-insert time and differs per instance for the same op).
  origin_ts TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
);
CREATE INDEX IF NOT EXISTS idx_package_ops_wip
  ON package_ops(branch_id) WHERE commit_hash IS NULL;
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied
  ON package_ops(applied) WHERE applied = 0;
CREATE INDEX IF NOT EXISTS idx_package_ops_commit_hash ON package_ops(commit_hash);
-- Partial index on propagation_id for efficient lookups (only non-null values)
CREATE INDEX IF NOT EXISTS idx_package_ops_propagation_id
  ON package_ops(propagation_id) WHERE propagation_id IS NOT NULL;


CREATE TABLE IF NOT EXISTS branch_ops (
  id TEXT PRIMARY KEY,                  -- content-addressed hash of the op
  op_blob BLOB NOT NULL,                -- serialized BranchOp
  applied INTEGER NOT NULL DEFAULT 0,   -- 0=pending, 1=applied (for crash recovery)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_branch_ops_created_at ON branch_ops(created_at);


--------------------
-- Package projections (content-addressed)
--------------------
-- Definitions stored once per content hash; locations is the
-- branch-scoped name-resolution layer pointing at hashes.

CREATE TABLE IF NOT EXISTS package_types (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS package_values (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_dval BLOB,                  -- NULL until evaluated
  value_type BLOB,               -- for finding values of a given ValueType
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_package_values_type ON package_values(value_type);

CREATE TABLE IF NOT EXISTS package_functions (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Content-addressed bytes (Blob refs). Dedup comes for free via PK
-- uniqueness; orphans reclaimed by `LibDB.RuntimeTypes.Blob.sweepOrphans`.
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);


-- Branch-scoped name resolution: maps (owner, modules, name) → hash.
-- `unlisted_at` tracks pointer-lifecycle (renames, propagation,
-- WIP→committed swaps); separate from author-initiated `deprecations`.
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,                -- 'fn' | 'type' | 'value'
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),  -- NULL = WIP
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP NULL,             -- set when a later row supersedes this one
  -- The origin_ts of the op that set THIS binding — the name→authoring-time mapping that lets
  -- playback order by CREATION, not arrival (timestamp-LWW). A SetName whose op was created
  -- EARLIER than the current binding (an old op arriving late via sync) is stale: playback skips
  -- the rebind, so the latest-by-creation name wins on every instance regardless of sync order.
  origin_ts TEXT NULL
);
CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(branch_id, owner, modules, name, item_type)
  WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_wip
  ON locations(branch_id) WHERE commit_hash IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);
CREATE INDEX IF NOT EXISTS idx_locations_commit_hash
  ON locations(commit_hash);


-- Author-initiated deprecations. Branch-scoped projection of
-- Deprecate / Undeprecate ops.
CREATE TABLE IF NOT EXISTS deprecations (
  deprecation_id TEXT PRIMARY KEY,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),  -- NULL = WIP
  item_hash TEXT NOT NULL,
  item_kind TEXT NOT NULL,                    -- 'fn' | 'type' | 'value'

  -- 'deprecated' (annotation_blob has kind + message + optional replacement ref)
  -- 'undeprecated' (annotation_blob NULL) — used for ancestor-override on child branches
  state TEXT NOT NULL,
  annotation_blob BLOB,

  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP                       -- set when a later row supersedes this one
);
CREATE INDEX IF NOT EXISTS idx_deprecations_lookup
  ON deprecations(branch_id, item_hash, item_kind) WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_deprecations_wip
  ON deprecations(branch_id) WHERE commit_hash IS NULL;
CREATE INDEX IF NOT EXISTS idx_deprecations_commit_hash
  ON deprecations(commit_hash);


-- Dependency edges between package items. Records the user-typed FQN
-- alongside the content hash so two distinct FQNs that share a hash
-- (e.g. multiple `val ... = 200L`) are tracked as separate edges
-- rather than collapsed by an INSERT OR IGNORE on hash alone.
--
-- The UNIQUE INDEX wraps the location columns in `COALESCE(col, '')`
-- because SQLite treats NULLs as distinct in a UNIQUE index without it,
-- which would let duplicate NULL-location rows accumulate on every
-- `updateDependencies` reinsert.
--
-- Fully derived from package items — no PK; uniqueness is enforced by
-- the index below. Rebuilt freely (no data preservation needed).
CREATE TABLE IF NOT EXISTS package_dependencies (
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

-- package_caps — content-addressed cache of a fn's effective capabilities (see PackageCaps.fs).
-- `caps` = newline-joined sorted grant-specs; '' = pure.
CREATE TABLE IF NOT EXISTS package_caps (
  hash TEXT PRIMARY KEY,
  caps TEXT NOT NULL
);


--------------------
-- Traces
--------------------

-- One row per handler invocation. Handler input (parsed dval bound to
-- the handler's parameter — `request` for HTTP, `expression` for eval)
-- lives directly here; no separate one-row-per-trace inputs table.
-- `input_value` is a binary-serialized RT.Dval (LibSerialization.Binary).
CREATE TABLE IF NOT EXISTS traces (
  id TEXT PRIMARY KEY,
  root_tlid INTEGER NOT NULL,
  handler_desc TEXT NOT NULL,
  timestamp TEXT NOT NULL,
  input_name TEXT NOT NULL,
  input_value BLOB NOT NULL,
  account_id TEXT REFERENCES accounts_v0(id)  -- NULL for unattributed (anonymous) runs
);


-- Every fn call AND every lambda invocation gets one row, linked via
-- parent_call_id (NULL for source-level entries). `kind` discriminates
-- function / lambda / builtin so the renderer can tag without
-- inspecting fn_hash.
--
-- `args` is a binary-serialized RT.Dval — a `DList(Unknown, …)` of
-- the call's arguments. `result` is the call's return Dval. Both go
-- through `LibSerialization.Binary.RT.Dval.serialize` / `.deserialize`.
--
-- function and lambda frames get real `duration_ms`; builtins remain
-- at 0 since the recorder only sees their synchronous storeFnResult,
-- with no matching entry hook.
CREATE TABLE IF NOT EXISTS trace_fn_calls (
  trace_id TEXT NOT NULL,
  call_id TEXT NOT NULL,
  parent_call_id TEXT,                       -- NULL for source-level
  kind TEXT NOT NULL,                        -- 'function' | 'lambda' | 'builtin'
  fn_hash TEXT,                              -- callee for function/builtin
  lambda_expr_id TEXT,                       -- AST id of the lambda body
  args BLOB NOT NULL,
  result BLOB NOT NULL,
  duration_ms INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);


--------------------
-- User-space (apps)
--------------------
-- Single-instance Dark: one DB per process, no per-scope key.
-- Test isolation is handled by wiping these tables between tests
-- (`TestUtils.initializeTestCanvas` truncates), not by scoping rows.

-- User K/V DBs (the runtime backing for `Stdlib.DB.*`).
CREATE TABLE IF NOT EXISTS user_data_v0 (
  id TEXT PRIMARY KEY,
  table_tlid INTEGER NOT NULL,
  user_version INTEGER NOT NULL,
  dark_version INTEGER NOT NULL,
  data TEXT NOT NULL,                       -- JSON stored as text
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  key TEXT NOT NULL,
  UNIQUE (table_tlid, dark_version, user_version, key)
);
CREATE INDEX IF NOT EXISTS idx_user_data_fetch
  ON user_data_v0 (table_tlid, user_version, dark_version);
CREATE INDEX IF NOT EXISTS idx_user_data_current_data_for_tlid
  ON user_data_v0 (user_version, dark_version, table_tlid);
-- (No GIN index equivalent in SQLite.)
CREATE INDEX IF NOT EXISTS idx_user_data_json ON user_data_v0 (data);


-- Top-levels (legacy). TODO split into a few tables (dbs, handlers, etc).
CREATE TABLE IF NOT EXISTS toplevels_v0 (
  tlid INTEGER NOT NULL PRIMARY KEY,
  digest CHAR(32) NOT NULL,
  tipe TEXT NOT NULL CHECK (tipe IN ('db', 'handler')),
  name TEXT,                                -- handlers only — used for HTTP lookups
  module TEXT,                              -- handlers only
  modifier TEXT,                            -- handlers only
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  deleted INTEGER NOT NULL CHECK (deleted IN (0, 1)),
  data BLOB NOT NULL
);


-- User-saved CLI scripts (managed via `dark scripts ...`).
CREATE TABLE IF NOT EXISTS scripts_v0 (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  text TEXT NOT NULL
);


--------------------
-- Sync (local-only setup; NOT synced) — see LibDB/{Remotes,SyncCursors,Conflicts}.fs
--------------------

-- Registered sync peers (managed via `dark remote ...`). Each row is a (name, url) the
-- tailnet sync daemon polls.
CREATE TABLE IF NOT EXISTS sync_remotes (
  name TEXT PRIMARY KEY,
  url TEXT NOT NULL
);

-- Per-remote poll resume state: how far this instance has folded each peer's op stream.
-- The cursor is a `package_ops` rowid (SQLite's monotonic insertion order).
CREATE TABLE IF NOT EXISTS sync_cursors (
  remote TEXT PRIMARY KEY,
  folded_through_rowid INTEGER NOT NULL DEFAULT 0
);

-- The recorded, reviewable log of auto-resolved name-binding divergences (`dark conflicts`).
-- Recorded at pull time; auto-resolved by policy (default last-writer-wins) but never silently lost.
CREATE TABLE IF NOT EXISTS sync_conflicts (
  id TEXT PRIMARY KEY,
  location TEXT NOT NULL,
  local_hash TEXT NOT NULL,
  incoming_hash TEXT NOT NULL,
  resolution TEXT NOT NULL,
  remote TEXT NOT NULL,
  detected_at TEXT NOT NULL DEFAULT (datetime('now')),
  acknowledged INTEGER NOT NULL DEFAULT 0,
  overridden INTEGER NOT NULL DEFAULT 0
);

-- Structured telemetry from the autosync daemon: one row per poll cycle, so `sync events` (and a
-- future dashboard view) can show activity as DATA rather than scraping the text log. Local-only,
-- never synced; trimmed to the most recent rows so it stays bounded.
CREATE TABLE IF NOT EXISTS sync_daemon_events (
  id INTEGER PRIMARY KEY,
  at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  peers_polled INTEGER NOT NULL,
  changed INTEGER NOT NULL,
  conflicts INTEGER NOT NULL,
  skews INTEGER NOT NULL
);
