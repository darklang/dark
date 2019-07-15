CREATE TABLE IF NOT EXISTS
access
( access_account UUID NOT NULL
, organization_account UUID NOT NULL
, permission VARCHAR(255) NOT NULL
)
;

CREATE UNIQUE INDEX access_organization
  ON access(access_account, organization_account)
