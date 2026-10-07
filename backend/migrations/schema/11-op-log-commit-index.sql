-- Ops by the commit that holds them. `02-op-log.sql` indexes only the draft (`commit_hash IS NULL`),
-- so asking how many ops one commit holds scanned the whole log, and `dark commits` asks that about
-- every row it lists. That file is frozen, so the index lives here.
CREATE INDEX IF NOT EXISTS idx_package_ops_commit ON package_ops(commit_hash);
