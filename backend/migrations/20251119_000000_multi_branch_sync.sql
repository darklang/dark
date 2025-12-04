
ALTER TABLE package_ops ADD COLUMN instance_id TEXT NULL
  REFERENCES instances(id) ON DELETE SET NULL;

-- Create index for querying ops by instance
CREATE INDEX IF NOT EXISTS idx_package_ops_instance
  ON package_ops(instance_id) WHERE instance_id IS NOT NULL;
