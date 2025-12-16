-- Approvals system: Allow users to propose changes to namespaces they don't own

-- Approval requests: groups pending locations for review (like a PR)
CREATE TABLE IF NOT EXISTS approval_requests (
  id TEXT PRIMARY KEY,
  created_by TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  status TEXT NOT NULL DEFAULT 'open', -- 'open', 'closed'
  target_namespace TEXT NOT NULL,
  source_branch_id TEXT NULL,
  title TEXT NULL,
  description TEXT NULL
);

CREATE INDEX IF NOT EXISTS idx_approval_requests_created_by ON approval_requests(created_by);
CREATE INDEX IF NOT EXISTS idx_approval_requests_target_namespace ON approval_requests(target_namespace);
CREATE INDEX IF NOT EXISTS idx_approval_requests_status ON approval_requests(status);


-- Location approvals: tracks review status of each location in a request
CREATE TABLE IF NOT EXISTS location_approvals (
  approval_request_id TEXT NOT NULL,
  location_id TEXT NOT NULL,
  content_hash TEXT NOT NULL,                -- hash of content when submitted
  status TEXT NOT NULL DEFAULT 'pending',    -- 'pending', 'approved', 'rejected', 'changes_requested'
  status_message TEXT NULL,                  -- reason for rejected, comment for changes_requested
  reviewed_by TEXT NULL,                     -- account_id of reviewer
  reviewed_at TIMESTAMP NULL,
  PRIMARY KEY (approval_request_id, location_id),
  FOREIGN KEY (approval_request_id) REFERENCES approval_requests(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_location_approvals_status ON location_approvals(status);
CREATE INDEX IF NOT EXISTS idx_location_approvals_location ON location_approvals(location_id);

-- Indexes for approval queries on locations table
CREATE INDEX IF NOT EXISTS idx_locations_approval_status ON locations(approval_status);
CREATE INDEX IF NOT EXISTS idx_locations_created_by ON locations(created_by);

-- Namespace access: controls who can approve changes to a namespace
CREATE TABLE IF NOT EXISTS namespace_access (
  namespace TEXT NOT NULL,
  account_id TEXT NOT NULL,
  role TEXT NOT NULL,  -- 'owner', 'reviewer'
  granted_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  granted_by TEXT NOT NULL,  -- account_id of who granted access
  PRIMARY KEY (namespace, account_id)
);

CREATE INDEX IF NOT EXISTS idx_namespace_access_account ON namespace_access(account_id);
CREATE INDEX IF NOT EXISTS idx_namespace_access_namespace ON namespace_access(namespace);

-- Seed initial accounts
INSERT OR IGNORE INTO accounts (id, username) VALUES ('Darklang', 'Darklang');
INSERT OR IGNORE INTO accounts (id, username) VALUES ('Stachu', 'Stachu');
INSERT OR IGNORE INTO accounts (id, username) VALUES ('Feriel', 'Feriel');

-- Seed initial namespace ownership
INSERT OR IGNORE INTO namespace_access (namespace, account_id, role, granted_by) VALUES ('Darklang', 'Darklang', 'owner', 'Darklang');
INSERT OR IGNORE INTO namespace_access (namespace, account_id, role, granted_by) VALUES ('Stachu', 'Stachu', 'owner', 'Stachu');
INSERT OR IGNORE INTO namespace_access (namespace, account_id, role, granted_by) VALUES ('Feriel', 'Feriel', 'owner', 'Feriel');

-- Add owner column to branches table
ALTER TABLE branches ADD COLUMN owner TEXT NOT NULL DEFAULT '';
-- Create index on branches owner
CREATE INDEX IF NOT EXISTS idx_branches_owner ON branches(owner);
-- Enforce unique branch names per owner
CREATE UNIQUE INDEX IF NOT EXISTS idx_branches_owner_name ON branches(owner, name);
