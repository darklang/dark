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
-- - track who's done what -- see adjacent migration around accounts+access
-- - implement some sort of tracking about what has/hasn't been synced upwards
-- - hmm we likely need an ops table or two...


-- Drop old package tables
DROP TABLE IF EXISTS package_types_v0;
DROP TABLE IF EXISTS package_values_v0;
DROP TABLE IF EXISTS package_functions_v0;


-- Create new package content tables (no location info)
CREATE TABLE package_types (
  id TEXT PRIMARY KEY, -- (UUID)
  -- TODO hash
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE TABLE package_values (
  id TEXT PRIMARY KEY, -- (UUID)
  -- TODO hash
  pt_def BLOB NOT NULL,
  rt_dval BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE TABLE package_functions (
  id TEXT PRIMARY KEY, -- (UUID)
  -- TODO hash
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);



-- Locations table: maps (owner, modules, name) -> item_id
-- Supports branch-specific overrides and deprecation tracking
CREATE TABLE locations (
  id TEXT PRIMARY KEY,              -- Item ID (references package_types/package_values/package_functions)

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

CREATE INDEX idx_locations_lookup ON locations(owner, modules, name, branch_id)
  WHERE deprecated_at IS NULL;
CREATE INDEX idx_locations_module ON locations(owner, modules)
  WHERE deprecated_at IS NULL;
CREATE INDEX idx_locations_conflicts ON locations(owner, modules, name, branch_id)
  WHERE deprecated_at IS NULL AND branch_id IS NOT NULL;

-- Index for branch shadowing/override lookups (used in NOT EXISTS subqueries)
-- This helps find if a branch-specific version exists for a given location
CREATE INDEX idx_locations_shadowing ON locations(owner, modules, name, item_type, branch_id, deprecated_at);

-- Index for branch_id filtering to speed up branch-scoped searches
CREATE INDEX idx_locations_branch ON locations(branch_id) WHERE branch_id IS NOT NULL;


CREATE TABLE branches (
  id TEXT PRIMARY KEY,
  created_by TEXT NULL REFERENCES accounts(id),   -- Who created this branch (for attribution)
  title TEXT NOT NULL,                            -- User-friendly name (non-unique)
  state TEXT NOT NULL,                            -- 'active', 'merged', 'abandoned'
  created_at TIMESTAMP NOT NULL,
  last_active_at TIMESTAMP NOT NULL,
  merged_at TIMESTAMP NULL
);

CREATE INDEX idx_branches_state ON branches(state);
CREATE INDEX idx_branches_active ON branches(last_active_at) WHERE state = 'active';

-- Track which branch each account is currently working on
CREATE TABLE account_context (
  account_id TEXT PRIMARY KEY REFERENCES accounts(id),
  current_branch_id TEXT REFERENCES branches(id),
  last_updated_at TIMESTAMP NOT NULL
);

-- Default 'anon' account for local development
INSERT INTO accounts (id, name, created_at)
VALUES ('00000000-0000-0000-0000-000000000001', 'anon', datetime('now'))
ON CONFLICT DO NOTHING;


-- Ops tracking table - source of truth for all package changes
-- The package_types/package_values/package_functions/locations tables are projections of these ops
CREATE TABLE package_ops (
  id TEXT PRIMARY KEY,
  branch_id TEXT NULL,              -- NULL = merged to main, non-null = branch-specific
  op_blob BLOB NOT NULL,            -- Serialized PackageOp (includes type: AddType/AddValue/AddFn/Set*Name)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX idx_package_ops_branch ON package_ops(branch_id);
CREATE INDEX idx_package_ops_created ON package_ops(created_at);

-- FUTURE: Patches/commits within branches
-- CREATE TABLE patches (
--   id UUID PRIMARY KEY,
--   branch_id UUID NOT NULL REFERENCES branches(id),
--   title TEXT NOT NULL,
--   author_id UUID NOT NULL REFERENCES accounts(id),
--   created_at TIMESTAMP NOT NULL,
--   updated_at TIMESTAMP NOT NULL,
--   merged_at TIMESTAMP NULL,        -- NULL = not merged yet
--   merged_by UUID NULL REFERENCES accounts(id)
-- );
-- CREATE INDEX idx_patches_author ON patches(author_id);
-- CREATE INDEX idx_patches_branch ON patches(branch_id);
-- CREATE INDEX idx_patches_merged ON patches(merged_at);

-- CLEANUP can/should the concept of a 'package item' be extracted out somehow?
-- would that somehow help dealing with unparseables?




