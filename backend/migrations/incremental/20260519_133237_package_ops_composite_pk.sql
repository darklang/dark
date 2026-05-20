-- Ghost-function fix: PK was bare `id` (op content hash), so an
-- identical op on two branches hashed equal and the second INSERT was
-- dropped by `INSERT OR IGNORE` — fn invisible everywhere though `fn`
-- printed `✓ Created`. PK becomes (id, branch_id), so IGNORE only
-- catches true within-branch repeats.

DROP TABLE package_ops;

CREATE TABLE package_ops (
  id TEXT NOT NULL,
  op_blob BLOB NOT NULL,
  branch_id TEXT NOT NULL REFERENCES branches(id),
  commit_hash TEXT REFERENCES commits(hash),
  applied INTEGER NOT NULL DEFAULT 0,
  propagation_id TEXT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (id, branch_id)
);

CREATE INDEX IF NOT EXISTS idx_package_ops_wip
  ON package_ops(branch_id) WHERE commit_hash IS NULL;
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied
  ON package_ops(applied) WHERE applied = 0;
CREATE INDEX IF NOT EXISTS idx_package_ops_commit_hash ON package_ops(commit_hash);
CREATE INDEX IF NOT EXISTS idx_package_ops_propagation_id
  ON package_ops(propagation_id) WHERE propagation_id IS NOT NULL;
