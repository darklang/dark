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
-- Before executing a test, `dark test` compiles its stored body into interpreter instructions.
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
