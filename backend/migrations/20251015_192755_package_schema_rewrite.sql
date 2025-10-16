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


CREATE TABLE branches (
  id TEXT PRIMARY KEY,
  owner_id TEXT NOT NULL REFERENCES accounts(id),  -- Who owns this branch
  title TEXT NOT NULL,                             -- User-friendly name
  state TEXT NOT NULL,                             -- 'active', 'merged', 'abandoned'
  is_default BOOLEAN NOT NULL DEFAULT FALSE,       -- Default branch for account
  created_at TIMESTAMP NOT NULL,
  last_active_at TIMESTAMP NOT NULL,
  merged_at TIMESTAMP NULL
);

CREATE INDEX idx_branches_owner ON branches(owner_id, state);
CREATE INDEX idx_branches_state ON branches(state);
CREATE INDEX idx_branches_active ON branches(last_active_at) WHERE state = 'active';
CREATE INDEX idx_branches_default ON branches(owner_id) WHERE is_default = TRUE;

-- **Default branch per account**:
-- - First branch created by account is automatically marked `is_default = TRUE`
-- - Only one default branch per owner_id (enforced in application logic)
-- - VS Code defaults to default branch on startup
-- - CLI shows `* stachu/main (default)` indicator for default branch
-- - Normal UUIDs used (not nil UUID to avoid collisions across accounts)


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




