-- Add hash column to package_types_v0
ALTER TABLE package_types_v0
  ADD COLUMN hash TEXT;

-- Add hash column to package_values_v0
ALTER TABLE package_values_v0
  ADD COLUMN hash TEXT;

-- Add hash column to package_functions_v0
ALTER TABLE package_functions_v0
  ADD COLUMN hash TEXT;

-- Create indexes for efficient hash-based lookups
CREATE INDEX idx_package_types_hash
  ON package_types_v0(hash)
  WHERE hash IS NOT NULL;

CREATE INDEX idx_package_values_hash
  ON package_values_v0(hash)
  WHERE hash IS NOT NULL;

CREATE INDEX idx_package_functions_hash
  ON package_functions_v0(hash)
  WHERE hash IS NOT NULL;