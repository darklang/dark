CREATE TYPE toplevel_type AS
ENUM ('handler', 'db', 'user_function');

CREATE TABLE
toplevel_oplists
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, account_id UUID REFERENCES accounts(id) NOT NULL
, tlid INTEGER NOT NULL
, digest CHAR(32) NOT NULL
, tipe toplevel_type
, name TEXT /* handlers only - used for http lookups */
, module TEXT /* handlers only */
, modifier TEXT /* handlers only */
, updated_at TIMESTAMP NOT NULL DEFAULT NOW()
, created_at TIMESTAMP NOT NULL DEFAULT NOW()
, data BYTEA NOT NULL
, PRIMARY KEY (canvas_id, tlid));

CREATE TRIGGER set_toplevel_oplist_timestamp
BEFORE UPDATE ON toplevel_oplists
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
