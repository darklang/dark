CREATE TABLE
json_oplists
( host VARCHAR(64)
, digest CHAR(32)
, updated_at TIMESTAMP NOT NULL DEFAULT NOW()
, created_at TIMESTAMP NOT NULL DEFAULT NOW()
, data TEXT
, PRIMARY KEY (host));

DROP TRIGGER IF EXISTS set_json_oplist_timestamp
ON json_oplists;

CREATE TRIGGER set_json_oplist_timestamp
BEFORE UPDATE ON json_oplists
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
