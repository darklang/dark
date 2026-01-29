-- SCM Commits: Replace is_wip with proper commit tracking

-- Create commits table
CREATE TABLE IF NOT EXISTS commits (
  id TEXT PRIMARY KEY,
  message TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_commits_created ON commits(created_at);

-- Add commit_id to package_ops (nullable - NULL means WIP)
ALTER TABLE package_ops ADD COLUMN commit_id TEXT REFERENCES commits(id);

-- Migrate existing committed ops (is_wip = 0) to a legacy commit
INSERT INTO commits (id, message, created_at)
SELECT
  '00000000-0000-0000-0000-000000000001',
  'Legacy: pre-SCM committed ops',
  COALESCE(MIN(created_at), datetime('now'))
FROM package_ops
WHERE is_wip = 0
HAVING COUNT(*) > 0;

UPDATE package_ops
SET commit_id = '00000000-0000-0000-0000-000000000001'
WHERE is_wip = 0;

-- Add commit_id to locations table
ALTER TABLE locations ADD COLUMN commit_id TEXT REFERENCES commits(id);

UPDATE locations
SET commit_id = '00000000-0000-0000-0000-000000000001'
WHERE is_wip = 0;

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_package_ops_commit ON package_ops(commit_id);
CREATE INDEX IF NOT EXISTS idx_package_ops_wip ON package_ops(commit_id) WHERE commit_id IS NULL;
CREATE INDEX IF NOT EXISTS idx_locations_commit ON locations(commit_id);
