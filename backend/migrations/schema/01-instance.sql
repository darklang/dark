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
