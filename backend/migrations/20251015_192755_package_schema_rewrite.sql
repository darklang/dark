-- Package storage schema
-- Content tables store definitions (content-addressed)
-- Locations table maps names to content (branch-scoped)
-- Package ops are the source of truth for all changes (branch-scoped)

-- Branches table
CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,

  -- (only) `main` has NULL here
  parent_branch_id TEXT REFERENCES branches(id),

  -- fork point on parent
  -- note: the FK is added after `commits` table is added
  base_commit_id TEXT,

  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  merged_at TIMESTAMP -- NULL until merged
);

-- Well-known main branch
INSERT OR IGNORE INTO branches (id, name) VALUES ('89282547-e4e6-4986-bcb6-db74bc6a8c0f', 'main');


-- Commits table
CREATE TABLE IF NOT EXISTS commits (
  id TEXT PRIMARY KEY,
  message TEXT NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_commits_branch ON commits(branch_id, created_at DESC);


-- Content tables store definitions
-- (content-addressed, no naming, no branch)
CREATE TABLE IF NOT EXISTS package_types (
  id TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS package_values (
  id TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_dval BLOB,  -- NULL until evaluated. CLEANUP once we're bootstrapped, can be tidied
  value_type BLOB, -- for finding values of a given ValueType
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_package_values_type ON package_values(value_type);

CREATE TABLE IF NOT EXISTS package_functions (
  id TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  rt_instrs BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);


-- Locations table: branch-scoped name resolution
-- Maps (owner, modules, name) -> item_id within a branch context
CREATE TABLE IF NOT EXISTS locations (
  location_id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,  -- 'fn', 'type', or 'value'
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_id TEXT REFERENCES commits(id),  -- NULL = WIP
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  deprecated_at TIMESTAMP NULL
);

CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(branch_id, owner, modules, name, item_type)
  WHERE deprecated_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE deprecated_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_wip
  ON locations(branch_id)
  WHERE commit_id IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_owner_modules
  ON locations(owner, modules);

CREATE INDEX IF NOT EXISTS idx_locations_commit ON locations(commit_id);


-- Package ops: the source of truth for all package changes (branch-scoped)
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT PRIMARY KEY,
  op_blob BLOB NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_id TEXT REFERENCES commits(id),  -- NULL = WIP
  applied INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_package_ops_wip
  ON package_ops(branch_id)
  WHERE commit_id IS NULL;

CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied ON package_ops(applied) WHERE applied = 0;
CREATE INDEX IF NOT EXISTS idx_package_ops_commit ON package_ops(commit_id);

-- Dependency tracking
CREATE TABLE IF NOT EXISTS package_dependencies (
  item_id TEXT NOT NULL,
  depends_on_id TEXT NOT NULL,
  PRIMARY KEY (item_id, depends_on_id)
);

CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on ON package_dependencies(depends_on_id);
CREATE INDEX IF NOT EXISTS idx_package_dependencies_item ON package_dependencies(item_id);
