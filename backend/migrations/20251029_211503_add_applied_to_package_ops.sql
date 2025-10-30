-- Add applied column to track whether ops have been processed
-- Default to TRUE for existing ops (assume already applied)
-- New ops default to FALSE until they're applied

ALTER TABLE package_ops ADD COLUMN applied BOOLEAN NOT NULL DEFAULT FALSE;

-- Mark all existing ops as applied
UPDATE package_ops SET applied = TRUE;

-- Add index for querying unapplied ops
CREATE INDEX IF NOT EXISTS idx_package_ops_applied ON package_ops(applied) WHERE applied = FALSE;
