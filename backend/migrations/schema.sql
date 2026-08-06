-- Schema for the Dark SQLite DB. ONE FILE -- Migrations.fs hashes it and kill-and-fills on change,
-- so each table is defined ONCE in its final form. system_migrations_v0 is the one exception, since
-- legacy DBs are adopted via that table; it is created here AND by Migrations.fs's adoptLegacyDB.
--
-- Order: bookkeeping, op log, package projections, locations, traces, user-data, toplevels,
-- scripts. FK targets precede FK sources so kill-and-fill replays cleanly.


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
-- These IDs are part of the API; don't rotate.
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES
  ('00000000-0000-0000-0000-000000000001', 'Darklang'),
  ('00000000-0000-0000-0000-000000000002', 'Stachu'),
  ('00000000-0000-0000-0000-000000000003', 'Paul'),
  ('00000000-0000-0000-0000-000000000004', 'Feriel');


--------------------
-- Op log
--------------------

-- The source of truth for all package changes: the content-addressed op log.
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT NOT NULL,
  op_blob BLOB NOT NULL,
  applied INTEGER NOT NULL DEFAULT 0,
  -- An op is always STORED (synced) but only takes EFFECT -- folds into the live projections, so it
  -- resolves and becomes callable -- when `effective = 1`. Locally-authored and trusted ops are
  -- effective on arrival; untrusted synced ops land `effective = 0`: present in the log, inert.
  effective INTEGER NOT NULL DEFAULT 1,
  -- The commit this op belongs to; NULL = DRAFT (edited and live, not yet committed). Gates nothing
  -- at runtime; it answers "what have I changed", which is what `dark status` reports.
  commit_hash TEXT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  -- Authoring timestamp, PORTABLE across sync. A locally-authored op self-stamps at insert; a SYNCED
  -- op preserves its origin, so every instance agrees and max(origin_ts) picks the same divergence
  -- winner. Distinct from `created_at`, which is local-insert time and differs per instance.
  origin_ts TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  -- The id is the op's content hash, so INSERT OR IGNORE dedups identical re-adds.
  PRIMARY KEY (id)
);
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied
  ON package_ops(applied) WHERE applied = 0;
-- Fast lookup of the "pending review" queue (synced but not yet effective).
CREATE INDEX IF NOT EXISTS idx_package_ops_pending
  ON package_ops(effective) WHERE effective = 0;
-- The draft: `dark status` asks this on every invocation, and it must stay cheap as the log grows.
CREATE INDEX IF NOT EXISTS idx_package_ops_draft
  ON package_ops(commit_hash) WHERE commit_hash IS NULL;

-- Who pushed which op, so a RELAY can serve "your stuff" back by identity. Many-to-many: a
-- content-addressed op can be pushed by several identities. `owner` is a NAME, not an authentication:
-- an unsigned string, trusted by convention, with nothing to stop one identity claiming another's.
-- Only a relay populates this; an instance ignores it.
CREATE TABLE IF NOT EXISTS op_owners (
  op_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  PRIMARY KEY (op_id, owner)
);
CREATE INDEX IF NOT EXISTS idx_op_owners_owner ON op_owners(owner);


-- A branch = a stable ID + an optional name alias + a FRONTIER of ops. Refer to a branch BY ID;
-- `name` is a mutable alias. A branch's authored ops are inserted `effective = 0` (present in the
-- shared log, NOT folded into main) and tagged in `op_branches`; its overlay package manager is
-- withExtraOps(core, the branch's ops), and MERGE up flips those ops `effective = 1` and folds.
CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,                 -- stable branch id (the handle); name is an alias
  name TEXT NOT NULL DEFAULT '',
  parent_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001',  -- branches off branches: the parent (default main)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  merged_at TIMESTAMP NULL,            -- set when the branch's work is merged into its parent
  archived_at TIMESTAMP NULL           -- set when the branch is archived (soft delete)
);
-- Deliberately NOT unique, not even among live branches. Two instances can each start a `fix-auth`
-- and after a sync BOTH rows live here: different branches that share a label, which is the entire
-- reason a branch id is a uuid, and a unique-name constraint would turn a sync into a failed import.
-- `resolveOrCreate` handles the local "don't start two under one name" race instead.
CREATE INDEX IF NOT EXISTS idx_branches_name ON branches(name) WHERE name != '';
CREATE INDEX IF NOT EXISTS idx_branches_parent ON branches(parent_id);

-- The per-branch frontier: which ops belong to a branch (many-to-many; an op can be shared).
CREATE TABLE IF NOT EXISTS op_branches (
  op_id TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  PRIMARY KEY (op_id, branch_id)
);
CREATE INDEX IF NOT EXISTS idx_op_branches_branch ON op_branches(branch_id);

-- The per-name BASE for a branch: main's content-hash for a name when the branch FIRST touched it
-- ('' if the name was new). Content hashes are STABLE across a reload (unlike origin_ts, which
-- reload re-stamps). A merge CONFLICT = main's CURRENT hash for the name differs from this base.
CREATE TABLE IF NOT EXISTS branch_name_bases (
  branch_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  base_hash TEXT NOT NULL,   -- main's item_hash for this name at first touch ('' = the name was new)
  PRIMARY KEY (branch_id, owner, modules, name)
);
CREATE INDEX IF NOT EXISTS idx_branch_name_bases_branch ON branch_name_bases(branch_id);

-- RELAY-side branch storage: a relay is store-and-forward, so it keeps pushed branch BUNDLES (the
-- export JSON) keyed by (owner, branch_id) and hands them back on pull. Only a relay populates it.
CREATE TABLE IF NOT EXISTS relay_branches (
  owner TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  bundle TEXT NOT NULL,
  PRIMARY KEY (owner, branch_id)
);
CREATE INDEX IF NOT EXISTS idx_relay_branches_owner ON relay_branches(owner);

-- COMMITS: the checkpoint record. Authoring is live-on-write, so a commit is NOT a gate -- an op
-- takes effect the moment it folds. A commit is the one human moment: review what changed, let
-- propagation run over the FINAL versions (so five edits to one fn collapse to one repoint),
-- surface conflicts, mark the dep-closed set. Ops with `commit_hash IS NULL` are the DRAFT.
CREATE TABLE IF NOT EXISTS commits (
  -- Content-derived, over the message, author, stamp, PARENT and the sorted ids of the ops it names.
  -- The same work committed onto the same history gets the same id on two machines; the same work onto
  -- different histories does not, the way a git commit's id depends on its parent.
  hash TEXT PRIMARY KEY,
  message TEXT NOT NULL DEFAULT '',
  author TEXT NOT NULL DEFAULT '',
  origin_ts TEXT NOT NULL,          -- portable authoring stamp, same clock as package_ops.origin_ts
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  parent TEXT NOT NULL DEFAULT ''   -- the commit this one follows; '' for the first in a store
);
CREATE INDEX IF NOT EXISTS idx_commits_created ON commits(created_at);


-- The per-name BASE for a SYNC SOURCE: the hash this instance and that source last AGREED on.
-- Mirrors branch_name_bases, keyed by source id (a relay url, or "file:<path>") -- a sync-import IS
-- a merge, the incoming ops being a delta against the state at the last common sync. So ONE
-- base-agnostic detector serves both, each asking "did BOTH sides move this name since the base?".
-- A MISSING row means no recorded agreement, which is NOT a conflict.
CREATE TABLE IF NOT EXISTS sync_bases (
  source_id TEXT NOT NULL,          -- the peer/relay identity (a url), or "file:<path>" for a file import
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  base_hash TEXT NOT NULL,          -- the agreed item_hash ('' = the name did not exist at the base)
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (source_id, owner, modules, name)
);
CREATE INDEX IF NOT EXISTS idx_sync_bases_source ON sync_bases(source_id);


-- CONFLICTS: recorded at RECONCILIATION (merge / rebase / sync-import) when two divergent op sets
-- both rebound the same name to DIFFERENT hashes since a common base. Reconciliation auto-resolves
-- deterministically (origin_ts LWW) AND records the conflict as 'pending', so the loser is not
-- silently gone: a human acks the auto-pick or overrides it. NOT written on the plain local
-- authoring path, since a sequential self-update is not a conflict.
CREATE TABLE IF NOT EXISTS conflicts (
  id TEXT PRIMARY KEY,              -- stable short id
  owner TEXT NOT NULL,              -- the location (name) in contention
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,
  kind TEXT NOT NULL,               -- 'same-name-different-hash' (room for cap-change / sig-break later)
  candidates TEXT NOT NULL,         -- JSON: [{ hash, origin_ts, author }] -- the competing versions
  auto_resolved_to TEXT NOT NULL,   -- the winning hash reconciliation picked
  reason TEXT NOT NULL,             -- 'later-origin-ts' | 'tie-break-by-hash' | ...
  status TEXT NOT NULL DEFAULT 'pending',  -- pending | acked | overridden
  resolved_by TEXT,                 -- resolution op id, set on override
  origin_ts TEXT,                   -- cross-instance LWW of the conflict record itself
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  -- Which branch this divergence is ON, spelled the way a branch id is spelled
  -- everywhere. Without it, settling a conflict from a branch would close a store-wide record while
  -- the fix lives in an overlay that does nothing until merge.
  branch_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001'
);
CREATE INDEX IF NOT EXISTS idx_conflicts_status ON conflicts(status);
CREATE INDEX IF NOT EXISTS idx_conflicts_branch ON conflicts(branch_id);
CREATE INDEX IF NOT EXISTS idx_conflicts_name ON conflicts(owner, modules, name);


--------------------
-- Propagation policy
--------------------
-- Which items follow a dependency when it moves, and which stay put. Ownership may inform the
-- DEFAULT, but an explicit row here always wins. Resolution is most-specific-first (item, then
-- module, then each parent module, then the default), the same shape name resolution has;
-- name = '' means a MODULE-level choice covering everything beneath it.
--
-- BRANCH-SCOPED, like everything else a branch can change. Main's id is the well-known uuid in
-- `ProgramTypes.BranchId.main`; do not spell it here. A branch
-- inherits main's choices and can override them. Sharing one row would leak branch state into main,
-- and silently, since nothing about a pin says which branch it came from.
--
-- Derived: folded from `Decide` ops and nothing else writes here, so this is a projection listed in
-- `Seed.projectionTables`. Drop it and the log rebuilds it.
CREATE TABLE IF NOT EXISTS propagation_policy (
  branch_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001',
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,               -- '' = a module-level choice
  policy TEXT NOT NULL,             -- 'follow' | 'pin'
  note TEXT,                        -- why, in the author's words; a pin without a reason ages badly
  origin_ts TEXT,                   -- cross-instance LWW, same as everything else
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (branch_id, owner, modules, name)
);


--------------------
-- Package projections (content-addressed)
--------------------
-- Definitions stored once per content hash; locations is the name-resolution layer over them.

CREATE TABLE IF NOT EXISTS package_types (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''         -- plain-text doc comment for SQL package search
);

CREATE TABLE IF NOT EXISTS package_values (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_dval BLOB,                  -- NULL until evaluated
  value_type BLOB,               -- for finding values of a given ValueType
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''         -- plain-text doc comment for SQL package search
);
CREATE INDEX IF NOT EXISTS idx_package_values_type ON package_values(value_type);

CREATE TABLE IF NOT EXISTS package_functions (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''         -- plain-text doc comment for SQL package search
);

-- Content-addressed bytes (Blob refs). Dedup comes for free via PK
-- uniqueness; orphans reclaimed by `LibDB.RuntimeTypes.Blob.sweepOrphans`.
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);


-- Op ids that arrived from a BUILD's embedded seed rather than being authored here or pulled from a
-- peer. Append-only and local: it records where an op came from, which no later fold can re-derive.
-- Comparing against the seed currently held answers a different question, and would call the whole
-- previous package set locally authored.
CREATE TABLE IF NOT EXISTS seed_ops (
  op_id TEXT PRIMARY KEY
);

-- Name resolution: maps (owner, modules, name) to a content hash. `unlisted_at` tracks
-- pointer-lifecycle (renames, propagation, WIP-to-committed swaps); separate from author-initiated
-- `deprecations`.
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,                -- 'fn' | 'type' | 'value'
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP NULL,             -- set when a later row supersedes this one
  -- The origin_ts of the op that set THIS binding, so playback orders by CREATION rather than
  -- arrival (timestamp-LWW). A SetName created EARLIER than the current binding (an old op arriving
  -- late via sync) is stale: playback skips the rebind, so the latest-by-creation name wins.
  origin_ts TEXT NULL,
  -- What put this binding here: 'op' = the normal op-fold (incl. WIP authoring), 'resolution' = a
  -- human/keep-local resolution OVERLAY. `discard` deletes op-fold bindings but excludes 'resolution',
  -- so it can't silently revert a synced resolution into a divergence.
  source TEXT NOT NULL DEFAULT 'op',
  -- The op that wrote this binding. `origin_ts` orders bindings but does not IDENTIFY one: two
  -- instances can mint the same stamp, so a lookup by stamp can land on the wrong op and misread
  -- "is this binding committed yet". This is what `pin` uses to tell a staged repoint from a
  -- committed one.
  op_id TEXT NOT NULL DEFAULT '',
  -- The hash this binding REPLACED, as the `SetName` op recorded it, or NULL when the op named no
  -- predecessor. Projected here because conflict detection needs it from BOTH sides: the incoming side
  -- reads it off the op, and without this column the local side always answered "none recorded", so the
  -- rule that two bindings replacing the SAME hash have diverged could never fire on a real pair.
  --
  -- Distinct from walking back through `origin_ts`: what an op SAYS it replaced is the lineage, and the
  -- previous row by stamp is only the previous row by stamp.
  previous TEXT NULL
);
CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(owner, modules, name, item_type)
  WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);
-- Hash -> name. Reads that join `locations` on `item_hash` are frequent, and without this SQLite
-- builds a throwaway index for each one: work proportional to the whole table, for a lookup.
CREATE INDEX IF NOT EXISTS idx_locations_item_hash
  ON locations(item_hash);


-- Author-initiated deprecations. Projection of Deprecate / Undeprecate ops.
CREATE TABLE IF NOT EXISTS deprecations (
  deprecation_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  item_kind TEXT NOT NULL,                    -- 'fn' | 'type' | 'value'

  -- 'deprecated' (annotation_blob has kind + message + optional replacement ref)
  -- 'undeprecated' (annotation_blob NULL), used for ancestor-override on child branches
  state TEXT NOT NULL,
  annotation_blob BLOB,

  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP                       -- set when a later row supersedes this one
);
CREATE INDEX IF NOT EXISTS idx_deprecations_lookup
  ON deprecations(item_hash, item_kind) WHERE unlisted_at IS NULL;


-- Dependency edges between package items. Records the user-typed FQN alongside the content hash, so
-- two distinct FQNs sharing a hash (e.g. multiple `val ... = 200L`) stay separate edges rather than
-- collapse under an INSERT OR IGNORE on hash alone. Fully derived from package items: no PK,
-- uniqueness enforced by the index below, and rebuilt freely.
--
-- The UNIQUE INDEX wraps the location columns in `COALESCE(col, '')` because SQLite treats NULLs as
-- distinct in a UNIQUE index without it, which would let duplicate NULL-location rows accumulate on
-- every `updateDependencies` reinsert.
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

-- package_caps: content-addressed cache of a fn's effective capabilities (see PackageCaps.fs).
-- `caps` = newline-joined sorted grant-specs; '' = pure.
CREATE TABLE IF NOT EXISTS package_caps (
  hash TEXT PRIMARY KEY,
  caps TEXT NOT NULL
);


--------------------
-- Traces
--------------------

-- One row per handler invocation. Handler input (the parsed dval bound to the handler's parameter:
-- `request` for HTTP, `expression` for eval) lives directly here, as a binary-serialized RT.Dval.
CREATE TABLE IF NOT EXISTS traces (
  id TEXT PRIMARY KEY,
  root_tlid INTEGER NOT NULL,
  handler_desc TEXT NOT NULL,
  timestamp TEXT NOT NULL,
  input_name TEXT NOT NULL,
  input_value BLOB NOT NULL,
  account_id TEXT REFERENCES accounts_v0(id)  -- NULL for unattributed (anonymous) runs
);


-- Every fn call AND every lambda invocation gets one row, linked via parent_call_id (NULL for
-- source-level entries). `kind` discriminates function / lambda / builtin so the renderer can tag
-- without inspecting fn_hash. `args` is a binary-serialized RT.Dval, a `DList` of the call's
-- arguments; `result` is the return Dval.
--
-- function and lambda frames get real `duration_ms`; builtins stay at 0, since the recorder only
-- sees their synchronous storeFnResult and there is no matching entry hook.
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
-- Single-instance Dark: one DB per process, no per-scope key. Test isolation comes from wiping these
-- tables between tests (`TestUtils.initializeTestCanvas` truncates), not from scoping rows.

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
  name TEXT,                                -- handlers only, used for HTTP lookups
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

-- Mutable, per-install local config: the CLI entry-point pointer (`entry_point`) plus per-user
-- settings. Deliberately NOT content-addressed and NOT synced -- sync ships ops, never this table.
CREATE TABLE IF NOT EXISTS config_v0 (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
