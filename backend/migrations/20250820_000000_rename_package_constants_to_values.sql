CREATE TABLE package_values_v0
( id TEXT PRIMARY KEY
, owner TEXT NOT NULL -- e.g. Darklang
, modules TEXT NOT NULL -- e.g. Math.Geometry
, name TEXT NOT NULL -- e.g. pi
, pt_def BLOB -- serialized PT.PackageValue
, rt_dval BLOB -- serialized RT.PackageValue
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_package_values_pt_def
  ON package_values_v0(id) WHERE pt_def IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_package_values_rt_dval
  ON package_values_v0(id) WHERE rt_dval IS NOT NULL;

-- Drop the old table
DROP TABLE package_constants_v0;