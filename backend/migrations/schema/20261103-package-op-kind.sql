-- The schema runner guards ADD COLUMN so replay is safe on existing stores.
ALTER TABLE package_ops ADD COLUMN op_kind TEXT NOT NULL DEFAULT 'unknown';

-- Queries can find renames without decoding every operation.
CREATE INDEX IF NOT EXISTS idx_package_ops_kind ON package_ops(op_kind);

-- Also find operations that left no location row because a newer draft won.
CREATE INDEX IF NOT EXISTS idx_locations_op_id ON locations(op_id);
