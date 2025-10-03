-- Remove id columns from package tables and use (owner, modules, name) as primary key
-- Keep hash as a regular indexed column for structure-based lookups

-- Recreate package_types_v0 with composite primary key
DROP TABLE package_types_v0;
CREATE TABLE package_types_v0 (
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  hash TEXT NOT NULL,
  pt_def BLOB,
  rt_def BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (owner, modules, name)
);

-- Recreate package_values_v0 with composite primary key
DROP TABLE package_values_v0;
CREATE TABLE package_values_v0 (
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  hash TEXT NOT NULL,
  pt_def BLOB,
  rt_dval BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (owner, modules, name)
);

-- Recreate package_functions_v0 with composite primary key
DROP TABLE package_functions_v0;
CREATE TABLE package_functions_v0 (
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  hash TEXT NOT NULL,
  pt_def BLOB,
  rt_instrs BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (owner, modules, name)
);

-- Create indexes for efficient hash-based lookups
CREATE INDEX IF NOT EXISTS idx_package_types_hash ON package_types_v0(hash);
CREATE INDEX IF NOT EXISTS idx_package_values_hash ON package_values_v0(hash);
CREATE INDEX IF NOT EXISTS idx_package_functions_hash ON package_functions_v0(hash);