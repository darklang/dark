-- Remove id columns from package tables and make hash the primary key

-- Recreate package_types_v0 with hash as primary key
DROP TABLE package_types_v0;
CREATE TABLE package_types_v0 (
  hash TEXT PRIMARY KEY,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  pt_def BLOB,
  rt_def BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Recreate package_values_v0 with hash as primary key  
DROP TABLE package_values_v0;
CREATE TABLE package_values_v0 (
  hash TEXT PRIMARY KEY,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  pt_def BLOB,
  rt_dval BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Recreate package_functions_v0 with hash as primary key
DROP TABLE package_functions_v0;
CREATE TABLE package_functions_v0 (
  hash TEXT PRIMARY KEY,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  pt_def BLOB,
  rt_instrs BLOB,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Create indexes for efficient lookups
CREATE INDEX IF NOT EXISTS idx_package_types_name ON package_types_v0(owner, modules, name);
CREATE INDEX IF NOT EXISTS idx_package_values_name ON package_values_v0(owner, modules, name);
CREATE INDEX IF NOT EXISTS idx_package_functions_name ON package_functions_v0(owner, modules, name);