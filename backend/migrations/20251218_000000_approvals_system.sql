-- Approvals system: Allow users to propose changes to namespaces they don't own

-- Seed initial accounts
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES ('00000000-0000-0000-0000-000000000001', 'Darklang');
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES ('00000000-0000-0000-0000-000000000002', 'Stachu');
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES ('00000000-0000-0000-0000-000000000003', 'Feriel');

-- Approval requests: groups pending items for review (like a PR)
CREATE TABLE IF NOT EXISTS approval_requests (
  id TEXT PRIMARY KEY,
  created_by TEXT NOT NULL,  -- UUID referencing accounts_v0(id)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  status TEXT NOT NULL DEFAULT 'open', -- 'open', 'closed'
  target_namespace TEXT NOT NULL,
  source_branch_id TEXT NULL,
  title TEXT NULL,
  description TEXT NULL,
  FOREIGN KEY (created_by) REFERENCES accounts_v0(id)
);

CREATE INDEX IF NOT EXISTS idx_approval_requests_created_by ON approval_requests(created_by);
CREATE INDEX IF NOT EXISTS idx_approval_requests_target_namespace ON approval_requests(target_namespace);
CREATE INDEX IF NOT EXISTS idx_approval_requests_status ON approval_requests(status);


-- Request items: tracks review status of each item in a request
CREATE TABLE IF NOT EXISTS request_items (
  request_id TEXT NOT NULL,
  location_id TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',    -- 'pending', 'approved', 'rejected', 'changes_requested'
  status_message TEXT NULL,                  -- reason for rejected, comment for changes_requested
  reviewed_by TEXT NULL,                     -- UUID referencing accounts_v0(id)
  reviewed_at TIMESTAMP NULL,
  PRIMARY KEY (request_id, location_id),
  FOREIGN KEY (request_id) REFERENCES approval_requests(id) ON DELETE CASCADE,
  FOREIGN KEY (reviewed_by) REFERENCES accounts_v0(id)
);

CREATE INDEX IF NOT EXISTS idx_request_items_status ON request_items(status);
CREATE INDEX IF NOT EXISTS idx_request_items_location ON request_items(location_id);

-- Add approval columns to locations table
-- created_by is UUID referencing accounts_v0(id)
ALTER TABLE locations ADD COLUMN created_by TEXT NOT NULL DEFAULT '';
ALTER TABLE locations ADD COLUMN approval_status TEXT NOT NULL DEFAULT 'approved';
ALTER TABLE locations ADD COLUMN reviewed_by TEXT NULL;  -- UUID of reviewer
ALTER TABLE locations ADD COLUMN reviewed_at TIMESTAMP NULL;

-- Indexes for approval queries on locations table
CREATE INDEX IF NOT EXISTS idx_locations_approval_status ON locations(approval_status);
CREATE INDEX IF NOT EXISTS idx_locations_created_by ON locations(created_by);

-- Namespace access: controls who can approve changes to a namespace
-- account_id and granted_by reference accounts_v0(id) as UUIDs
CREATE TABLE IF NOT EXISTS namespace_access (
  namespace TEXT NOT NULL,
  account_id TEXT NOT NULL,  -- UUID referencing accounts_v0(id)
  role TEXT NOT NULL,  -- 'owner', 'reviewer'
  granted_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  granted_by TEXT NOT NULL,  -- UUID referencing accounts_v0(id)
  PRIMARY KEY (namespace, account_id),
  FOREIGN KEY (account_id) REFERENCES accounts_v0(id),
  FOREIGN KEY (granted_by) REFERENCES accounts_v0(id)
);

CREATE INDEX IF NOT EXISTS idx_namespace_access_account ON namespace_access(account_id);
CREATE INDEX IF NOT EXISTS idx_namespace_access_namespace ON namespace_access(namespace);

-- Seed initial namespace ownership (using UUIDs)
INSERT OR IGNORE INTO namespace_access (namespace, account_id, role, granted_by)
  VALUES ('Darklang', '00000000-0000-0000-0000-000000000001', 'owner', '00000000-0000-0000-0000-000000000001');
INSERT OR IGNORE INTO namespace_access (namespace, account_id, role, granted_by)
  VALUES ('Stachu', '00000000-0000-0000-0000-000000000002', 'owner', '00000000-0000-0000-0000-000000000002');
INSERT OR IGNORE INTO namespace_access (namespace, account_id, role, granted_by)
  VALUES ('Feriel', '00000000-0000-0000-0000-000000000003', 'owner', '00000000-0000-0000-0000-000000000003');

-- Add owner column to branches table (UUID referencing accounts_v0(id))
ALTER TABLE branches ADD COLUMN owner TEXT NOT NULL DEFAULT '';
-- Create index on branches owner
CREATE INDEX IF NOT EXISTS idx_branches_owner ON branches(owner);
-- Enforce unique branch names per owner
CREATE UNIQUE INDEX IF NOT EXISTS idx_branches_owner_name ON branches(owner, name);
