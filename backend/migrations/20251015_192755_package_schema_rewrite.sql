-- Rewrites how we store and access all 'package matter'
--
-- Prior to this migration, there were just 3 tables:
-- `package_types_v0`, `package_values_v0`, `package_fns_v0
-- These included all basic stuff -- id, name/location, PT, RT.
--
-- Now that we're working on source control and package syncing,
-- this simple approach has hit its limit.
--
-- This migration aims to:
-- - split item storage from 'naming' mechanism
-- - introduce 'branches' to manage work in progress
-- - implement tracking about what has/hasn't been synced upwards


-- Drop old package tables
DROP TABLE IF EXISTS package_types_v0;
DROP TABLE IF EXISTS package_values_v0;
DROP TABLE IF EXISTS package_functions_v0;


-- Create new package content tables (no location info)
CREATE TABLE IF NOT EXISTS package_types (
  id TEXT PRIMARY KEY, -- (UUID)
  -- TODO hash
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE TABLE IF NOT EXISTS package_values (
  id TEXT PRIMARY KEY, -- (UUID)
  -- TODO hash
  pt_def BLOB NOT NULL,
  rt_dval BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE TABLE IF NOT EXISTS package_functions (
  id TEXT PRIMARY KEY, -- (UUID)
  -- TODO hash
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);



-- Locations table: maps (owner, modules, name) -> item_id
-- Supports branch-specific overrides and deprecation tracking
CREATE TABLE  IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,    -- Unique ID for this location entry
  item_id TEXT NOT NULL,            -- Item ID (references package_types/package_values/package_functions)

  branch_id TEXT NULL,              -- NULL = merged (main), non-null = branch-specific

  owner TEXT NOT NULL,              -- "Darklang"
  modules TEXT NOT NULL,            -- JSON array or dot-separated: "Stdlib.List"
  name TEXT NOT NULL,               -- "map"
  item_type TEXT NOT NULL,          -- 'fn', 'type', 'value'
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  deprecated_at TIMESTAMP NULL      -- NULL = active, non-null = deprecated
  -- NO unique constraint on (owner, modules, name, branch_id)
  -- Multiple branches can have conflicting locations (conflict detection needed at merge time)
);

CREATE INDEX IF NOT EXISTS idx_locations_lookup ON locations(owner, modules, name, branch_id)
  WHERE deprecated_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_module ON locations(owner, modules)
  WHERE deprecated_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_conflicts ON locations(owner, modules, name, branch_id)
  WHERE deprecated_at IS NULL AND branch_id IS NOT NULL;

-- Index for branch shadowing/override lookups (used in NOT EXISTS subqueries)
-- This helps find if a branch-specific version exists for a given location
CREATE INDEX IF NOT EXISTS idx_locations_shadowing ON locations(owner, modules, name, item_type, branch_id, deprecated_at);

-- Index for branch_id filtering to speed up branch-scoped searches
CREATE INDEX IF NOT EXISTS idx_locations_branch ON locations(branch_id) WHERE branch_id IS NOT NULL;


CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,                             -- User-friendly name (non-unique)
  created_at TIMESTAMP NOT NULL,
  merged_at TIMESTAMP NULL
);

CREATE INDEX IF NOT EXISTS idx_branches_name ON branches(name);


-- Ops tracking table - source of truth for all package changes
-- The package_types/package_values/package_functions/locations tables are projections of these ops
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT PRIMARY KEY,
  branch_id TEXT NULL,              -- NULL = merged to main, non-null = branch-specific
  op_blob BLOB NOT NULL,            -- Serialized PackageOp (includes type: AddType/AddValue/AddFn/Set*Name)
  --created_by TEXT NOT NULL REFERENCES account(id)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_package_ops_branch ON package_ops(branch_id);
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);


-- CLEANUP can/should the concept of a 'package item' be extracted out somehow?
-- would that somehow help dealing with unparseables?




-- Create instances table to track remote Darklang instances for syncing
CREATE TABLE IF NOT EXISTS instances (
  id TEXT PRIMARY KEY,          -- UUID
  name TEXT NOT NULL UNIQUE,    -- Human-readable name (e.g., "local", "production", "stachu-laptop")
  url TEXT NOT NULL             -- Base URL for the instance (e.g., "http://dark-packages.dlio.localhost:11001")
);

-- Index for looking up instances by name
CREATE INDEX IF NOT EXISTS idx_instances_name ON instances(name);
