-- Package storage schema
-- Content tables store definitions (content-addressed)
-- Locations table maps names to content
-- Package ops are the source of truth for all changes

-- Drop old package tables (from initial migration)
DROP TABLE IF EXISTS package_types_v0;
DROP TABLE IF EXISTS package_values_v0;
DROP TABLE IF EXISTS package_functions_v0;

-- Content tables store definitions (content-addressed, no naming)
CREATE TABLE IF NOT EXISTS package_types (
  id TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS package_values (
  id TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_dval BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS package_functions (
  id TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Locations table: name resolution
-- Maps (owner, modules, name) -> item_id
-- is_wip: FALSE = committed, TRUE = work in progress
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL, -- 'fn', 'type', or 'value'
  is_wip INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  deprecated_at TIMESTAMP NULL
);

-- Indexes for name lookups
CREATE INDEX IF NOT EXISTS idx_locations_lookup
  ON locations(owner, modules, name, is_wip) WHERE deprecated_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE deprecated_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_wip
  ON locations(is_wip) WHERE is_wip = 1;
CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);

-- Package ops: the source of truth for all package changes
-- Other tables (types/values/functions/locations) are projections
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT PRIMARY KEY,
  op_blob BLOB NOT NULL,
  is_wip INTEGER NOT NULL DEFAULT 1, -- new ops are WIP by default
  applied INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_package_ops_wip ON package_ops(is_wip);
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied ON package_ops(applied) WHERE applied = 0;
