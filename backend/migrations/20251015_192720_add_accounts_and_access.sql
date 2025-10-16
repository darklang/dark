-- Adds initial `accounts` and `access` tables
-- (so far unused -- just referenced)

-- accounts
CREATE TABLE accounts (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,       -- "Stachu", "Ocean", "Darklang"
  created_at TIMESTAMP NOT NULL
);

CREATE INDEX idx_accounts_name ON accounts(name);


-- access
CREATE TABLE access (
  id UUID PRIMARY KEY,
  grantor_id UUID NOT NULL REFERENCES accounts(id),  -- Darklang grants access
  grantee_id UUID NOT NULL REFERENCES accounts(id),  -- to Stachu
  access_level TEXT NOT NULL,                        -- 'read' | 'write' | 'admin'
  created_at TIMESTAMP NOT NULL,

  UNIQUE(grantor_id, grantee_id)
);

CREATE INDEX idx_access_grantee ON access(grantee_id);
CREATE INDEX idx_access_grantor ON access(grantor_id);