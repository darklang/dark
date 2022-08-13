CREATE TABLE IF NOT EXISTS
registered_tunnelhosts
( user_id UUID NOT NULL
, tunnel_host VARCHAR(255) NOT NULL
, created_at TIMESTAMP NOT NULL DEFAULT NOW()
, updated_at TIMESTAMP NOT NULL DEFAULT NOW()
, PRIMARY KEY (user_id, tunnel_host)
);

CREATE TRIGGER set_registered_tunnelhosts_timestamp
BEFORE UPDATE ON registered_tunnelhosts
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
