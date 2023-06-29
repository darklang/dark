CREATE TABLE IF NOT EXISTS
package_constants_v0
-- IDs
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
, owner TEXT NOT NULL
, modules TEXT NOT NULL /* eg Twitter.Other; includes package name, but not owner name */
, fnname TEXT NOT NULL /* eg sendText */
, version INTEGER NOT NULL /* eg 0 */
-- the actual definition
, definition BYTEA NOT NULL /* the whole thing serialized as binary */
-- bonus
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TRIGGER set_package_constants_timestamp
BEFORE UPDATE ON package_constants_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
