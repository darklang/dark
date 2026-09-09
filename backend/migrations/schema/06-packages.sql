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


-- Op ids that arrived from a BUILD's embedded seed rather than authored here or pulled from a peer.
-- Append-only and local: provenance that no later fold can re-derive.
CREATE TABLE IF NOT EXISTS seed_ops (
  op_id TEXT PRIMARY KEY
);

-- Name resolution: maps (owner, modules, name) to a content hash. `unlisted_at` tracks
-- pointer-lifecycle (renames, propagation, WIP-to-committed swaps); separate from author-initiated
