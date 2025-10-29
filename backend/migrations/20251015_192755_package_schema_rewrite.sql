-- Rewrites package storage to support branches and sync tracking
-- Old design: 3 tables with id/name/PT/RT combined
-- New design: separate content (types/values/functions) from naming (locations)

-- Drop old package tables
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

-- Locations table maps (owner, modules, name) to item_id
-- Supports branch overrides and deprecation tracking
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  branch_id TEXT NULL, -- NULL = main branch
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL, -- 'fn', 'type', 'value'
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  deprecated_at TIMESTAMP NULL
);

CREATE INDEX IF NOT EXISTS idx_locations_lookup ON locations(owner, modules, name, branch_id)
  WHERE deprecated_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_module ON locations(owner, modules)
  WHERE deprecated_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_conflicts ON locations(owner, modules, name, branch_id)
  WHERE deprecated_at IS NULL AND branch_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_locations_shadowing ON locations(owner, modules, name, item_type, branch_id, deprecated_at);
CREATE INDEX IF NOT EXISTS idx_locations_branch ON locations(branch_id) WHERE branch_id IS NOT NULL;


CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL,
  merged_at TIMESTAMP NULL
);

CREATE INDEX IF NOT EXISTS idx_branches_name ON branches(name);

-- Package ops are the source of truth; other tables are projections
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT PRIMARY KEY,
  branch_id TEXT NULL,
  op_blob BLOB NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_package_ops_branch ON package_ops(branch_id);
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);

-- Instances table tracks remote Darklang instances for syncing
CREATE TABLE IF NOT EXISTS instances (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  url TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_instances_name ON instances(name);
