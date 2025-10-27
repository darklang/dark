CREATE TABLE IF NOT EXISTS syncs (
  id TEXT PRIMARY KEY,                    -- UUID
  instance_id TEXT NOT NULL,              -- UUID reference to instances table
  synced_at TIMESTAMP NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
  ops_pushed INTEGER NOT NULL DEFAULT 0,  -- Count of ops sent to remote instance
  ops_fetched INTEGER NOT NULL DEFAULT 0, -- Count of ops received from remote instance
  FOREIGN KEY (instance_id) REFERENCES instances(id) ON DELETE CASCADE
);


-- Recreate indexes
CREATE INDEX IF NOT EXISTS idx_syncs_instance ON syncs(instance_id, synced_at DESC);
CREATE INDEX IF NOT EXISTS idx_syncs_recent ON syncs(synced_at DESC);
