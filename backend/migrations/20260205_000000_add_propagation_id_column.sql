-- Add propagation_id column to package_ops table for direct lookup of PropagateUpdate ops
ALTER TABLE package_ops ADD COLUMN propagation_id TEXT NULL;

-- Create partial index on propagation_id for efficient lookups (only index non-null values)
CREATE INDEX IF NOT EXISTS idx_package_ops_propagation_id
  ON package_ops(propagation_id) WHERE propagation_id IS NOT NULL;
