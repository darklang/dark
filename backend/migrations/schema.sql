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
-- Order: bookkeeping, op log, package projections, locations, traces,
-- user-data, toplevels, scripts. FK targets precede FK sources.
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
-- Op log
--------------------

-- The source of truth for all package changes: the content-addressed op log.
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT NOT NULL,
  op_blob BLOB NOT NULL,
  applied INTEGER NOT NULL DEFAULT 0,
  -- Sync vs playback split (step 4): an op is always STORED (synced) but only takes EFFECT -- folds into the
  -- live projections, so it resolves + becomes callable -- when `effective = 1`. Locally-authored ops and
  -- ops from trusted sources are effective on arrival; untrusted synced ops land `effective = 0` (present in
  -- the log, inert) until approved. Default 1 keeps the single-user store byte-identical to pre-step-4.
  effective INTEGER NOT NULL DEFAULT 1,
  -- The commit this op belongs to; NULL = DRAFT (edited and live, not yet committed). Live-on-write means
  -- this gates nothing at runtime -- it only answers "what have I changed since I last looked at it",
  -- which is what `dark status` reports and `dark commit` answers.
  commit_hash TEXT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  -- Authoring timestamp, PORTABLE across sync. A
  -- locally-authored op self-stamps here at insert; a SYNCED op preserves its origin (the sync
  -- receiver writes the peer's value), so every instance agrees on a given op's origin_ts and
  -- max(origin_ts) picks the same divergence winner → no swap. Distinct from `created_at` (which
  -- is local-insert time and differs per instance for the same op).
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

-- Who pushed which op, so a RELAY can serve "your stuff" back to you by identity. Many-to-
-- many: an op (content-addressed) can be pushed by several identities. FAKE AUTH for now --
-- `owner` is an unsigned identity string, trusted by convention; signed identity + trust is
-- the later approval layer. Only a relay populates this (on push); an instance ignores it.
CREATE TABLE IF NOT EXISTS op_owners (
  op_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  PRIMARY KEY (op_id, owner)
);
CREATE INDEX IF NOT EXISTS idx_op_owners_owner ON op_owners(owner);


-- Branches (concurrency pivot). A branch = a stable ID + an optional name alias + a FRONTIER of ops.
-- Refer to a branch BY ID; `name` is a mutable alias. The op log (package_ops) is the shared, content-
-- addressed store; a branch's authored ops are inserted `effective = 0` (present in the log, NOT folded
-- into main) and tagged in `op_branches`. A branch's overlay PM = withExtraOps(core, the branch's ops).
-- MERGE up = flip those ops `effective = 1` + fold into main. So the existing effective gate already
-- models "in the log but not in main" = branch-pending; branches reuse it rather than a new mechanism.
CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,                 -- stable branch id (the handle); name is an alias
  name TEXT NOT NULL DEFAULT '',
  parent_id TEXT NOT NULL DEFAULT 'main',  -- branches off branches: the parent (default main)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  merged_at TIMESTAMP NULL,            -- set when the branch's work is merged into its parent
  archived_at TIMESTAMP NULL           -- set when the branch is archived (soft delete)
);
-- Deliberately NOT unique, not even among live branches. Two instances can each start a `fix-auth`, and
-- once they sync BOTH rows live here: they are different branches that happen to share a label, and
-- keeping them apart is the entire reason a branch id is a uuid. A unique-name constraint would turn an
-- ordinary sync into a failed import.
-- `resolveOrCreate` handles the local "don't start two under one name" race with a conditional insert
-- instead, which is a constraint on how WE mint branches rather than on what the table can hold.
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
-- (or '' if the name was new). This is the fork point per name -- content hashes are STABLE across a
-- reload (unlike origin_ts, which reload re-stamps), so it's a reliable "based-on" marker. A merge
-- CONFLICT = main's CURRENT hash for the name differs from this base (main changed it since the fork).
-- Replaces the timestamp watermark (branches.base_ts), which reload churn made meaningless.
CREATE TABLE IF NOT EXISTS branch_name_bases (
  branch_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  base_hash TEXT NOT NULL,   -- main's item_hash for this name at first touch ('' = the name was new)
  PRIMARY KEY (branch_id, owner, modules, name)
);
CREATE INDEX IF NOT EXISTS idx_branch_name_bases_branch ON branch_name_bases(branch_id);

-- RELAY-side branch storage: a relay is a dumb store-and-forward, so it keeps pushed branch BUNDLES
-- (the export JSON) keyed by (owner, branch_id) and hands them back on pull. Distinct from an
-- instance's own branches -- only a relay populates this (on branch-push). "branches follow me."
CREATE TABLE IF NOT EXISTS relay_branches (
  owner TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  bundle TEXT NOT NULL,
  PRIMARY KEY (owner, branch_id)
);
CREATE INDEX IF NOT EXISTS idx_relay_branches_owner ON relay_branches(owner);

-- COMMITS: the checkpoint record. Authoring is live-on-write, so a commit is NOT a gate -- an op takes effect
-- the moment it folds, committed or not. A commit is the one human moment: review what changed, let
-- propagation run over the FINAL versions (so five edits to one fn collapse to one repoint), surface any
-- conflicts, and mark the dep-closed set. Ops with `commit_hash IS NULL` are the DRAFT.
-- `origin_ts` is portable so a commit can travel like an op; membership is the nullable column on
-- package_ops, not a join table -- see QUESTIONS.md for what that trades away now that ops are
-- content-addressed and global.
CREATE TABLE IF NOT EXISTS commits (
  hash TEXT PRIMARY KEY,            -- content-derived, so the same commit gets the same id everywhere
  message TEXT NOT NULL DEFAULT '',
  author TEXT NOT NULL DEFAULT '',
  origin_ts TEXT NOT NULL,          -- portable authoring stamp, same clock as package_ops.origin_ts
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_commits_created ON commits(created_at);


-- The per-name BASE for a SYNC SOURCE: the hash this instance and that source last AGREED on. Exact mirror of
-- branch_name_bases, keyed by source id (a relay url, or "file:<path>") instead of branch id -- because a
-- sync-import IS a merge: the incoming ops are a delta against the state at the last common sync. Without this,
-- main-to-main sync has no base, so it can't tell "the peer updated a name I never touched" (not a conflict,
-- just take it) from "we both diverged from a common ancestor" (a conflict). With it, ONE base-agnostic
-- detector serves branch merge and sync import: both ask "did BOTH sides move this name since the base?".
-- A MISSING row means no recorded agreement, which is NOT a conflict -- guessing there is what would false-alarm
-- on every ordinary peer update.
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


-- CONFLICTS: recorded at RECONCILIATION (merge / rebase / sync-import) when two divergent op sets both rebound
-- the same name to DIFFERENT hashes since a common base. Reconciliation auto-resolves deterministically
-- (origin_ts LWW) AND records the conflict here as 'pending', so the loser is not silently gone -- a human acks
-- the auto-pick or overrides it (a resolution op) later. Syncable via origin_ts. NOT written on the plain local
-- authoring path (a sequential self-update is not a conflict). v0 shape, expected to iterate; see
-- notes/fresh-arch/checkpoint-ux.md.
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
  -- Which branch this divergence is ON; '' is main, spelled the way a branch id is spelled everywhere.
  -- Without it, settling a conflict from a branch meant closing a store-wide record while writing the fix
  -- into an overlay where it does nothing until merge, so `ack` and `override` refused on a branch rather
  -- than do something incoherent. Existing stores get this column from `LibDB.Releases`.
  branch_id TEXT NOT NULL DEFAULT ''
);
CREATE INDEX IF NOT EXISTS idx_conflicts_status ON conflicts(status);
CREATE INDEX IF NOT EXISTS idx_conflicts_branch ON conflicts(branch_id);
CREATE INDEX IF NOT EXISTS idx_conflicts_name ON conflicts(owner, modules, name);


--------------------
-- Propagation policy
--------------------
-- Which items follow a dependency when it moves, and which stay put. Propagation runs on every edit, so
-- without this the cascade is a rule the machine applies to you; with it, the cascade is something you
-- choose. Ownership may inform the DEFAULT, but an explicit row here always wins -- the user decides.
--
-- Resolution is most-specific-first (item, then module, then each parent module, then the default), which is
-- the same shape name resolution already has, so it should read as familiar rather than as a new concept.
-- name = '' means the row is a MODULE-level choice covering everything beneath it.
--
-- Not branch-scoped, deliberately: "this reference is deliberate" is a property of the item, not of the
-- branch you happened to be on when you said so. A one-off "skip this one, just this once" is a different
-- thing and belongs to the commit that skips it.
-- BRANCH-SCOPED, like everything else that a branch can change. `branch_id = ''` is main. A branch inherits
-- main's choices and can override them; resolution takes the branch's row when it has one, main's otherwise.
--
-- Scoping rather than sharing is what keeps branch isolation honest. A pin made on a branch writing into a
-- shared row would leak branch state into main, which is precisely what branches exist to prevent -- and it
-- would do it silently, since nothing about the pin says which branch it came from.
--
-- Derived: rows are folded from `Decide` ops and nothing else writes here, so this is a projection and is
-- listed in `Seed.projectionTables`. Drop it and the log rebuilds it.
CREATE TABLE IF NOT EXISTS propagation_policy (
  branch_id TEXT NOT NULL DEFAULT '',  -- '' = main
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
-- Definitions stored once per content hash; locations is the
-- name-resolution layer pointing at hashes.

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


-- Name resolution: maps (owner, modules, name) to a content hash.
-- `unlisted_at` tracks pointer-lifecycle (renames, propagation,
-- WIP→committed swaps); separate from author-initiated `deprecations`.
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,                -- 'fn' | 'type' | 'value'
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP NULL,             -- set when a later row supersedes this one
  -- The origin_ts of the op that set THIS binding — the name→authoring-time mapping that lets
  -- playback order by CREATION, not arrival (timestamp-LWW). A SetName whose op was created
  -- EARLIER than the current binding (an old op arriving late via sync) is stale: playback skips
  -- the rebind, so the latest-by-creation name wins on every instance regardless of sync order.
  origin_ts TEXT NULL,
  -- What put this binding here: 'op' = the normal op-fold (incl. WIP authoring), 'resolution' = a
  -- human/keep-local resolution OVERLAY. `discard` deletes op-fold bindings but excludes 'resolution',
  -- so it can't silently revert a synced resolution into a divergence.
  source TEXT NOT NULL DEFAULT 'op',
  -- The op that wrote this binding. `origin_ts` above orders bindings, but it does not IDENTIFY one:
  -- two instances can mint the same stamp, and after a sync this store holds both, so a lookup by stamp
  -- can land on the wrong op and read the wrong answer to "is this binding committed yet". This is the
  -- exact link, and it is what `pin` uses to tell a staged repoint from a committed one.
  --
  -- Empty for a row written before the column existed. Nothing stays empty for long: this table is a
  -- projection, so the schema change that adds the column drops it and re-folds the log into it.
  op_id TEXT NOT NULL DEFAULT ''
);
CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(owner, modules, name, item_type)
  WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE unlisted_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);
-- Hash -> name. Both the standing constraints check (`dark status`, every prompt) and the branch-aware
-- hash-to-name reads join `locations` on `item_hash`, and without this SQLite builds a throwaway index for
-- it on EVERY such query -- work proportional to the whole table, repeated, for a lookup.
CREATE INDEX IF NOT EXISTS idx_locations_item_hash
  ON locations(item_hash);


-- Author-initiated deprecations. Projection of Deprecate / Undeprecate ops.
CREATE TABLE IF NOT EXISTS deprecations (
  deprecation_id TEXT PRIMARY KEY,
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
  ON deprecations(item_hash, item_kind) WHERE unlisted_at IS NULL;


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

-- Hashes whose type surface has already been checked and found clean, per build.
--
-- A pure cache, sound because an item's hash IS its content: "this hash type-checks" cannot stop being true
-- while the CHECKER stays the same. Editing an item produces a different hash, which is simply not in the
-- table yet.
--
-- That caveat is why `build_hash` is here. Without it the cache would outlive the checker that filled it,
-- so widening a rule later (descending into `match`, say) would silently never fire on anything already
-- marked clean -- new rules that quietly do nothing on existing code. Keying on the build means an upgrade
-- invalidates exactly what it should, with no version constant for anyone to remember to bump.
--
-- It exists because the check is whole-store (a mismatch that arrives by sync is as real as one you typed)
-- and `dark status` runs it on every invocation. Unkeyed, every `dark status` walked 4,000-odd functions
-- and took five seconds; with it, the first run after a build pays and the rest look only at what is new.
--
-- A projection: drop it and the next check refills it.
CREATE TABLE IF NOT EXISTS type_checked (
  item_hash TEXT NOT NULL,
  build_hash TEXT NOT NULL,
  PRIMARY KEY (item_hash, build_hash)
);
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

-- Mutable, per-install local config: the CLI entry-point pointer (`entry_point`) + per-user settings.
-- Deliberately NOT content-addressed and NOT synced — this is local mutable state (Globals), kept separate
-- from the immutable op log by design (sync ships ops, never this table). See notes/fresh-arch/50-mvp-clean.md.
CREATE TABLE IF NOT EXISTS config_v0 (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
