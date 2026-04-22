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
