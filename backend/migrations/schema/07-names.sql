-- The layers that say what content MEANS right now: which name holds it, what is deprecated,
-- what each item says about itself, and what calls what.

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
  item_kind TEXT NOT NULL,                    -- 'fn' | 'type' | 'value'

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


-- What an item, or one named part of it, currently says about itself. Projection of UpdateDoc ops.
--
-- The prose itself lives in the item's serialized declaration, where every reader already looks;
-- this table is the LWW REGISTER that decides whether an arriving op gets to rewrite it. Without a
-- register there is nowhere to keep the doc's own origin_ts, so a stale op arriving late overwrote
-- a newer one, and nothing could tell an edit that descends from what we hold (apply it) from one
-- made against a text we never had (a divergence, which gets a conflict row).
CREATE TABLE IF NOT EXISTS item_docs (
  item_hash TEXT NOT NULL,
  -- 'item' | 'record-field' | 'enum-case' | 'parameter': WHICH kind of part.
  part TEXT NOT NULL,
  -- WHICH part, by its name in the declaration; '' for the item's own doc.
  within TEXT NOT NULL,
  text TEXT NOT NULL,
  -- The origin_ts of the op that wrote this text, so the newest STATEMENT wins rather than the last
  -- to arrive.
  origin_ts TEXT NOT NULL,
  PRIMARY KEY (item_hash, part, within)
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
