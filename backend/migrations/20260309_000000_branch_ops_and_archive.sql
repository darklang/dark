-- Add archived_at to branches (replacing hard delete)
ALTER TABLE branches ADD COLUMN archived_at TIMESTAMP NULL;

-- BranchOps table
CREATE TABLE IF NOT EXISTS branch_ops (
  id TEXT PRIMARY KEY,                -- content-addressed hash of the op
  op_blob BLOB NOT NULL,              -- serialized BranchOp
  applied INTEGER NOT NULL DEFAULT 0, -- 0=pending, 1=applied (for crash recovery)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_branch_ops_created_at ON branch_ops(created_at);
