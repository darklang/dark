-- The shape a NEW store is born with: every table and index, in dependency order.
--
-- ONE file, edited in place. It used to be fourteen numbered subsystem files, each frozen once it
-- had merged, because the same filename had to mean the same thing in every store ever made from
-- it. That rule bought backward compatibility with stores this build can no longer open anyway:
-- the molt dropped the sync tables, the user-space tables and `commits.author`, which is more
-- change than any upgrade path was going to carry. So the history is cut here. A store older than
-- this file is not supported; delete it and start again, and your packages re-author from disk.
--
-- What did NOT change is the mechanism. `LibDB.Releases` still exists and still runs, and a schema
-- change from here on is still two halves: declare it here, for new stores, and add a step there,
-- for stores that already exist. `scripts/migrations/new` scaffolds both. The numbering is gone
-- because with one base file there is nothing to order -- and because two branches adding a file
-- both took the next number, which is how this directory came to hold `10-runs.sql` beside
-- `10-traits.sql` and two files called `11-`.
--
-- Statement order still matters within this file: a foreign key's target must be declared before
-- the table that references it. `Releases.applySchemaTables` runs the CREATE TABLEs and
-- `applySchemaIndexes` the indexes, in two passes, so an index may name a column declared anywhere
-- above it.


-- ---------------------------------------------------------------------------
-- from 01-instance.sql
-- ---------------------------------------------------------------------------

-- The from-scratch shape of the store, split by subsystem and applied in LEXICAL ORDER.
--
-- This directory declares what a NEW store looks like. It is not how a change reaches an EXISTING
-- one: every statement here is `CREATE TABLE IF NOT EXISTS` (or `INSERT OR IGNORE`), which no-ops
-- against a table that already exists, so a new COLUMN declared here never arrives anywhere. That
-- is what `LibDB.Releases` is for, and it is the only answer -- a step there looks at the store
-- before acting, which a raw SQL file cannot.
--
-- The files are concatenated in filename order and hashed as one string, so splitting differently
-- or reordering IS a change: the bootstrap notices, drops the regenerable projections and replays.
-- FK targets must therefore still precede FK sources, across files as well as within one.
--
-- Bookkeeping that belongs to the INSTALL rather than to any package: what migrations have run,
-- and the accounts a store knows about.

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

-- ---------------------------------------------------------------------------
-- from 02-op-log.sql
-- ---------------------------------------------------------------------------

-- The op log: the source of truth every projection below is derived from, plus the two indexes
-- that keep `dark status` cheap.

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

-- ---------------------------------------------------------------------------
-- from 03-branches.sql
-- ---------------------------------------------------------------------------

-- Branches: an id, the ops tagged onto it, and the per-name fork points. A branch is an OVERLAY
-- of ops, not a copy of the store.

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
-- Deliberately NOT unique, even among live branches: two instances can each start a `fix-auth`,
-- and after a sync both rows live here. `resolveOrCreate` handles the local
-- don't-start-two-under-one-name race.
CREATE INDEX IF NOT EXISTS idx_branches_name ON branches(name) WHERE name != '';
CREATE INDEX IF NOT EXISTS idx_branches_parent ON branches(parent_id);

-- The per-branch frontier: which ops belong to a branch (many-to-many; an op can be shared).
CREATE TABLE IF NOT EXISTS op_branches (
  op_id TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  -- What put this op on the branch: 'op' = authored, 'propagation' = a repoint that followed an edit,
  -- 'resolution' = a conflict override or a pin's rollback. The branch twin of `locations.source`;
  -- without it a branch cannot tell "you typed this" from "this followed something", so `status`,
  -- `undo` and `pin` could only answer about main. Local metadata: it does not travel in a bundle.
  source TEXT NOT NULL DEFAULT 'op',
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

-- ---------------------------------------------------------------------------
-- from 04-commits.sql
-- ---------------------------------------------------------------------------

-- Commits, and the per-source agreement a sync diffs against.

-- COMMITS: the checkpoint record. Authoring is live-on-write, so a commit is NOT a gate -- an op
-- takes effect the moment it folds. A commit is the one human moment: review what changed, let
-- propagation run over the FINAL versions (so five edits to one fn collapse to one repoint),
-- surface conflicts, mark the dep-closed set. Ops with `commit_hash IS NULL` are the DRAFT.
CREATE TABLE IF NOT EXISTS commits (
  -- Content-derived, over the message, stamp, PARENT and the sorted ids of the ops it names.
  -- The same work committed onto the same history gets the same id on two machines; the same work onto
  -- different histories does not, the way a git commit's id depends on its parent.
  hash TEXT PRIMARY KEY,
  message TEXT NOT NULL DEFAULT '',
  origin_ts TEXT NOT NULL,          -- portable authoring stamp, same clock as package_ops.origin_ts
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  parent TEXT NOT NULL DEFAULT ''   -- the commit this one follows; '' for the first in a store
);
CREATE INDEX IF NOT EXISTS idx_commits_created ON commits(created_at);

-- ---------------------------------------------------------------------------
-- from 05-conflicts.sql
-- ---------------------------------------------------------------------------

-- Reconciliation findings (`conflicts`), folded from ops rather than authoritative.

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
  -- WHICH part of a declaration, for a doc divergence: 'item', 'record-field:x', 'enum-case:X',
  -- 'parameter:0'. Empty for a name divergence, which is about the whole binding.
  part TEXT NOT NULL DEFAULT '',
  kind TEXT NOT NULL,               -- 'same-name-different-hash' (room for cap-change / sig-break later)
  candidates TEXT NOT NULL,         -- JSON: [{ hash, origin_ts, author }] -- the competing versions
  auto_resolved_to TEXT NOT NULL,   -- the winning hash reconciliation picked
  reason TEXT NOT NULL,             -- 'later-origin-ts' | 'tie-break-by-hash' | ...
  status TEXT NOT NULL DEFAULT 'pending',  -- pending | acked | overridden
  resolved_by TEXT,                 -- resolution op id, set on override
  origin_ts TEXT,                   -- cross-instance LWW of the conflict record itself
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  -- Which branch this divergence is ON (default main). Without it, settling a conflict from a branch
  -- would close a store-wide record while the fix lives in an unmerged overlay.
  branch_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001'
);
CREATE INDEX IF NOT EXISTS idx_conflicts_status ON conflicts(status);
CREATE INDEX IF NOT EXISTS idx_conflicts_branch ON conflicts(branch_id);
CREATE INDEX IF NOT EXISTS idx_conflicts_name ON conflicts(owner, modules, name);

-- ---------------------------------------------------------------------------
-- from 06-packages.sql
-- ---------------------------------------------------------------------------

-- The content-addressed package projections: one row per hash, no names anywhere.

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

-- Three things per function, from the two halves of the compiler.
--
--   pt_def        the code as written, for the editor and the pretty-printer
--   rt_instrs     the register machine, for the interpreter
--   debug_symbols which instruction produced which expression's value, for anything mapping
--                 the machine back to the source: a trace's `// = 140`, and later an error
--                 that can say which expression rather than which instruction
--
-- The third is its own column because running code never reads it and reading code always
-- does, so a run should not pay to carry it. It is nullable and read lazily for the same
-- reason.
--
-- What it deliberately does NOT hold, so nobody goes looking: source SPANS (the editor places
-- hints by counting lines from the function's header, so it needs none) and VARIABLE NAMES (a
-- frame already carries its arguments, and the names belong to the parameter list, which is
-- the better source because shadowing can give two registers one name).
CREATE TABLE IF NOT EXISTS package_functions (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  debug_symbols BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''         -- plain-text doc comment for SQL package search
);

-- Tests are package content that nothing can call. Stored as source only;
-- Before executing a test, `dark test run` compiles its stored body into interpreter instructions.
CREATE TABLE IF NOT EXISTS package_tests (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''
);

-- Content-addressed bytes (Blob refs). Dedup comes for free via PK
-- uniqueness; orphans reclaimed by `LibDB.RuntimeTypes.Blob.sweepOrphans`.
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);


-- Op ids that arrived from a BUILD's embedded seed rather than authored here or pulled from a peer.
-- Append-only and local: provenance that no later fold can re-derive.
CREATE TABLE IF NOT EXISTS seed_ops (
  op_id TEXT PRIMARY KEY
);

-- Name resolution: maps (owner, modules, name) to a content hash. `unlisted_at` tracks
-- pointer-lifecycle (renames, propagation, WIP-to-committed swaps); separate from author-initiated

-- ---------------------------------------------------------------------------
-- from 07-names.sql
-- ---------------------------------------------------------------------------

-- The layers that say what content MEANS right now: which name holds it, what is deprecated,
-- what each item says about itself, and what calls what.

-- `deprecations`.
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_hash TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,                -- 'fn' | 'type' | 'value' | 'trait' | 'impl' | 'test'
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP NULL,             -- set when a later row supersedes this one
  -- The origin_ts of the op that set THIS binding, so playback orders by CREATION rather than
  -- arrival (timestamp-LWW). A SetName created EARLIER than the current binding (an old op arriving
  -- late via sync) is stale: playback skips the rebind, so the latest-by-creation name wins.
  origin_ts TEXT NULL,
  -- What put this binding here: 'op' = the normal op-fold (incl. WIP authoring), 'resolution' = a
  -- human/keep-local resolution OVERLAY. `discard` deletes op-fold bindings but excludes 'resolution',
  -- so it can't silently revert a synced resolution into a divergence. 'unbind' = an Unbind's
  -- TOMBSTONE: unlisted from birth, stamped with the unbind's origin_ts, so a binding authored before
  -- the unbind but arriving after it can be told to stay stale. Never live; history reads skip it.
  source TEXT NOT NULL DEFAULT 'op',
  -- The op that wrote this binding. `origin_ts` orders bindings but does not IDENTIFY one: two
  -- instances can mint the same stamp, so a lookup by stamp can land on the wrong op and misread
  -- "is this binding committed yet". This is what `pin` uses to tell a staged repoint from a
  -- committed one.
  op_id TEXT NOT NULL DEFAULT '',
  -- The hash this binding REPLACED, as the `SetName` op recorded it; NULL when the op named no
  -- predecessor. Conflict detection needs it from BOTH sides (incoming reads it off the op, local
  -- reads it here). What an op SAYS it replaced is the lineage; the previous row by origin_ts is
  -- only the previous row by stamp.
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
  item_kind TEXT NOT NULL,                    -- 'fn' | 'type' | 'value' | 'trait' | 'impl' | 'test'

  -- 'deprecated' (annotation_blob has kind + message + optional replacement ref)
  -- 'undeprecated' (annotation_blob NULL), used for ancestor-override on child branches
  state TEXT NOT NULL,
  annotation_blob BLOB,

  -- The origin_ts of the op that said this, so the answer is the NEWEST statement rather than the
  -- last one to arrive. `created_at` is when this machine wrote the row; two machines folding the
  -- same two ops in different orders used to disagree about whether an item is deprecated.
  origin_ts TEXT,

  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at TIMESTAMP                       -- set when a later row supersedes this one
);
CREATE INDEX IF NOT EXISTS idx_deprecations_lookup
  ON deprecations(item_hash, item_kind) WHERE unlisted_at IS NULL;


-- What a NAME says about itself, when it says something other than its declaration does.
-- Projection of UpdateDoc ops.
--
-- Keyed on the LOCATION, not on content, because content is shared: ten names hold
-- `type ParseError = | BadFormat | OutOfRange`, and what `Int64.ParseError` means is not what
-- `UInt64.ParseError` means. The declaration's own `///` travels in the item and is the fallback;
-- this is how one name says something else.
--
-- Nothing is here for most names. A row exists only where somebody edited a doc.
CREATE TABLE IF NOT EXISTS location_docs (
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  -- 'item' | 'record-field' | 'enum-case' | 'parameter': WHICH kind of part.
  kind TEXT NOT NULL,
  -- WHICH part, by its name in the declaration ('' for the declaration's own doc). A parameter is
  -- its INDEX, since a parameter's name is not part of identity.
  within TEXT NOT NULL,
  text TEXT NOT NULL,
  -- The origin_ts of the op that wrote this text, so the newest STATEMENT wins rather than the last
  -- to arrive.
  origin_ts TEXT NOT NULL,
  PRIMARY KEY (owner, modules, name, kind, within)
);


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

-- ---------------------------------------------------------------------------
-- from 08-traces.sql
-- ---------------------------------------------------------------------------

-- Traces: observations of past runs. Deliberately outlive the code they observed.

--------------------
-- Traces
--------------------

-- One row per handler invocation. Handler input (the parsed dval bound to the handler's parameter:
-- `request` for HTTP, `expression` for eval) lives directly here, as a binary-serialized RT.Dval.
-- NB: the last columns of this table were, until the schema cut, added by release steps and
-- never declared here, so a fresh store got them from the step. They are declared now, in the
-- order `ALTER TABLE ADD COLUMN` appended them, so a new store and an existing one agree.
CREATE TABLE IF NOT EXISTS traces (
  id TEXT PRIMARY KEY,
  root_tlid INTEGER NOT NULL,
  handler_desc TEXT NOT NULL,
  timestamp TEXT NOT NULL,
  input_name TEXT NOT NULL,
  input_value BLOB NOT NULL,
  account_id TEXT REFERENCES accounts_v0(id),  -- NULL for unattributed (anonymous) runs
  status TEXT NOT NULL DEFAULT 'done',
  parent_id TEXT,
  parent_seq INTEGER,
  pinned INTEGER NOT NULL DEFAULT 0,
  updated TEXT NOT NULL DEFAULT '',
  entry_hash TEXT,
  result_value BLOB,
  duration_ms INTEGER NOT NULL DEFAULT 0,
  bytes INTEGER NOT NULL DEFAULT 0
);


-- Every fn call AND every lambda invocation gets one row, linked via parent_call_id (NULL for
-- source-level entries). `kind` discriminates function / lambda / builtin so the renderer can tag
-- without inspecting fn_hash. `args` is a binary-serialized RT.Dval, a `DList` of the call's
-- arguments; `result` is the return Dval.
--
-- function and lambda frames get real `duration_ms`; builtins stay at 0, since the recorder only
-- sees their synchronous storeFnResult and there is no matching entry hook.
-- NB: the last columns of this table were, until the schema cut, added by release steps and
-- never declared here, so a fresh store got them from the step. They are declared now, in the
-- order `ALTER TABLE ADD COLUMN` appended them, so a new store and an existing one agree.
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
  process_id TEXT NOT NULL DEFAULT '',
  seq INTEGER NOT NULL DEFAULT 0,
  ord INTEGER NOT NULL DEFAULT -1,
  PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);


-- How many times each loop in a run went round.
--
-- One row per loop, keyed by the lambda's own expression id. Not a tree, and not a row per
-- pass: this answers exactly one question, and it exists for exactly one reason.
--
-- A view recomputes what every line evaluated to by REPLAYING the run with its effects answered
-- from the log, so it can only show the passes it reaches. A run whose log was capped, or which
-- was suspended mid-loop, has passes that happened and that no re-running will show. Without
-- this the view reports "pass 3 of 3" about a loop that went round five times, which is a
-- confident lie in a debugging tool.
--
-- Every pass is counted, including passes that made no impure call. An earlier version stored a
-- row per FRAME and kept only the frames an effectful call sat under, which wrote thousands of
-- rows for one loop AND silently counted zero for a pure one.
--
-- Values are not here, and neither is the tree. Both come from the replay. If something ever
-- needs to read a trace's shape WITHOUT replaying it, this table is where that would go; traces
-- are transient, so widening it later costs nothing.
CREATE TABLE IF NOT EXISTS trace_loops (
  trace_id  TEXT NOT NULL,
  -- The lambda's own expression id. Sibling passes share it, which is what makes them one loop.
  call_site TEXT NOT NULL,
  passes    INTEGER NOT NULL,
  PRIMARY KEY (trace_id, call_site)
);

-- ---------------------------------------------------------------------------
-- from 09-user-data.sql
-- ---------------------------------------------------------------------------

-- Per-install local config. Nothing here is derived from the op log.

-- Mutable, per-install local config: the current branch, the sync settings, the `exec.*` and
-- `trace.*` knobs. Deliberately NOT content-addressed and NOT synced -- sync ships ops, never
-- this table.
CREATE TABLE IF NOT EXISTS config_v0 (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);

-- ---------------------------------------------------------------------------
-- from 10-runs.sql
-- ---------------------------------------------------------------------------

-- A trace IS a run, and this file is everything the run half of that needs that `08-traces.sql`
-- cannot hold. That file has merged, so it is frozen (`AGENTS.md`, "Changing the schema"): the
-- COLUMNS this PR adds to `traces` and `trace_fn_calls` therefore come from steps in
-- `LibDB/Releases.fs`, which is the only thing that can widen a table that already exists. What
-- CAN live here is a new table and any index, so both are here.

--------------------
-- Two things `08-traces.sql` says that are no longer true
--------------------

-- It is frozen, so this is where they get corrected rather than there.
--
-- `traces.root_tlid` is `NOT NULL` and every writer puts 0 in it. In classic it named the
-- handler whose trace this was; here a run is not always a handler invocation, and the question
-- it answered ("which handler served this?") is answered by `entry_hash`, which this PR adds.
-- Nothing reads it, there is no index on it, and a frozen `CREATE TABLE` cannot lose a column,
-- so it stays as a 4-byte-per-row fossil. Do not add a reader.
--
-- `trace_fn_calls`'s comment says every fn call and every lambda gets a row, that `kind`
-- discriminates function / lambda / builtin, and that builtins stay at `duration_ms = 0`. All
-- three are now wrong: only the impure calls are recorded, so `kind` is always 'builtin',
-- `parent_call_id` and `lambda_expr_id` are always NULL, and a builtin carries a real duration
-- (measured at the landing, for a read in flight). The columns stay for the same reason.


--------------------
-- Which functions a run went through: names only
--------------------

-- Recording keeps only the impure calls, so nothing in `trace_fn_calls` says
-- that a run passed through `MyApp.Orders.route`. This is what `dark traces calls <fn>` reads:
-- one row per (run, function), no arguments, no results, a few hundred bytes for a normal run.
--
-- `fn_hash` is the version the run actually went through. A resume replays the recorded log
-- against whatever those names mean NOW, so comparing the two is what lets `traces resume` say
-- which of the run's callees have been edited since.
CREATE TABLE IF NOT EXISTS trace_fns (
  trace_id TEXT NOT NULL,
  fn_name TEXT NOT NULL,
  fn_hash TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (trace_id, fn_name)
);
CREATE INDEX IF NOT EXISTS idx_trace_fns_fn_name ON trace_fns(fn_name);


--------------------
-- Indexes for the run verbs
--------------------

-- Indexes are applied in a pass of their own, after the release steps, so one may name a column
-- a step has just added (`Releases.applySchemaIndexes`). Every listing and retention's scan order
-- by `timestamp DESC`; `status` is how the CLI finds the runs that can be resumed.
CREATE INDEX IF NOT EXISTS idx_traces_timestamp ON traces(timestamp);
CREATE INDEX IF NOT EXISTS idx_traces_status    ON traces(status);

-- Every read of one run's log selects by `trace_id` and orders by `seq`, so the composite
-- serves both halves and the single-column `idx_trace_fn_calls_trace_id` in `08-traces.sql`
-- becomes redundant. That file is frozen, so the old index stays on disk; SQLite will pick
-- this one.
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_seq
  ON trace_fn_calls(trace_id, seq);

-- ---------------------------------------------------------------------------
-- from 10-traits.sql
-- ---------------------------------------------------------------------------

-- The two item kinds traits added: a trait, and an implementation of one for a type.
--
-- Its own file rather than an edit to `06-packages.sql`, because that file is already merged: a
-- schema file stops being editable the moment it lands on main, and a later change arrives as a
-- new file. Filename order is the contract (`LocalExec.Migrations.schemaSql`), and these tables
-- reference nothing, so sorting last is fine.

-- Traits and impls: their own item kinds, folded from AddTrait / AddTraitImpl. `trait_hash` on an
-- impl is the dispatch index: every impl of a trait is one indexed read.
CREATE TABLE IF NOT EXISTS package_traits (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS package_trait_impls (
  hash TEXT PRIMARY KEY,
  trait_hash TEXT NOT NULL,
  pt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT '',
  -- The origin_ts of the `AddTraitImpl` op that introduced this impl, so two impls of one trait
  -- for one type can be ordered the same way on every instance: the newer one is the one that
  -- runs (timestamp-LWW, `LibExecution.Lww`), and the older is reported as a rival rather than
  -- erroring at the call. Empty when the impl never came from an op (a script's own impls,
  -- grafted in memory): then there is no winner and the call still errors.
  origin_ts TEXT NOT NULL DEFAULT ''
);
CREATE INDEX IF NOT EXISTS idx_package_trait_impls_trait ON package_trait_impls(trait_hash);

-- ---------------------------------------------------------------------------
-- from 11-dependency-targets.sql
-- ---------------------------------------------------------------------------

-- Edges by the name and version they point at, for "which usages are a version behind"
-- (`SCM.Constraints.detectOutdatedUsages`), which `dark status` runs on every invocation.
--
-- Without it that query visits every edge in the store and looks each one's name up in
-- `locations`: about 32,000 lookups on a fresh store, to return nothing. With it the question
-- is asked per distinct (name, version) instead, from the index alone.
CREATE INDEX IF NOT EXISTS idx_package_dependencies_target
  ON package_dependencies(depends_on_owner, depends_on_modules, depends_on_name, depends_on_hash, item_hash)
  WHERE depends_on_owner IS NOT NULL;

-- ---------------------------------------------------------------------------
-- from 11-op-log-commit-index.sql
-- ---------------------------------------------------------------------------

-- Ops by the commit that holds them. `02-op-log.sql` indexes only the draft (`commit_hash IS NULL`),
-- so asking how many ops one commit holds scanned the whole log, and `dark commits` asks that about
-- every row it lists. That file is frozen, so the index lives here.
CREATE INDEX IF NOT EXISTS idx_package_ops_commit ON package_ops(commit_hash);

-- ---------------------------------------------------------------------------
-- from 12-trace-blobs.sql
-- ---------------------------------------------------------------------------

-- A trace's values by content hash, owned by the trace, written with it and deleted with it. Two
-- kinds, one table:
--
-- - every logged call's result, once per distinct value. `trace_fn_calls.result` holds the
--   value's hash as TEXT, and readers join here for the bytes (`08-traces.sql` is frozen, so the
--   column keeps its name). A traced type check reads the same items and gets the same small
--   answers thousands of times; one stdlib-wide trace had 8,217 calls and 2,700 distinct results.
-- - the blobs a trace captured: a request body, a file's bytes, anything an effect handed over as
--   a `Blob`.
--
-- They used to be promoted into `package_blobs`, which is shared with package values and user
-- data and which nothing collects, so retention removed the trace and kept the body forever:
-- five traced 400 KB POSTs left 2.0 MB there, and none of it was counted by `trace.maxMb`.
-- Content-addressed like `package_blobs`, so a read that misses there falls back to here
-- (`LibDB.RuntimeTypes.Blob.get`).
CREATE TABLE IF NOT EXISTS trace_blobs (
  trace_id TEXT NOT NULL,
  hash     TEXT NOT NULL,
  bytes    BLOB NOT NULL,
  PRIMARY KEY (trace_id, hash)
);
CREATE INDEX IF NOT EXISTS idx_trace_blobs_hash ON trace_blobs(hash);
