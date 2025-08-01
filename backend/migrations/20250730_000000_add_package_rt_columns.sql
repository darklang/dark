/*
For each of the `package_X` tables,
- rename PT column from `definition` to `pt_def`
- add new RT BLOB column to hold serialized data
  - `package_types_v0` gets `rt_def`
  - `package_constants_v0` gets `rt_dval`
  - `package_functions_v0` gets `rt_instrs`
- create indices for efficient RT data lookups

The new field is used to eliminate PT -> RT conversion at runtime.

The new columns are nullable as we plan on enabling 'lazy backfilling'
of the fields, since they can always be determined by what's in PT.
*/

-- types
ALTER TABLE package_types_v0
  RENAME COLUMN definition TO pt_def;

ALTER TABLE package_types_v0
  ADD COLUMN rt_def BLOB;

CREATE INDEX IF NOT EXISTS idx_package_types_rt_def
  ON package_types_v0(id) WHERE rt_def IS NOT NULL;


-- constants
ALTER TABLE package_constants_v0
RENAME COLUMN definition TO pt_def;

ALTER TABLE package_constants_v0
ADD COLUMN rt_dval BLOB;

CREATE INDEX IF NOT EXISTS idx_package_constants_rt_dval
  ON package_constants_v0(id) WHERE rt_dval IS NOT NULL;


-- functions
ALTER TABLE package_functions_v0
RENAME COLUMN definition TO pt_def;

ALTER TABLE package_functions_v0
ADD COLUMN rt_instrs BLOB;

CREATE INDEX IF NOT EXISTS idx_package_functions_rt_instrs
  ON package_functions_v0(id) WHERE rt_instrs IS NOT NULL;
