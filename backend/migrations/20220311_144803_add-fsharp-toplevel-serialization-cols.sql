-- These are the new ones for the F# serialization, that will live alongside the old ones for now.
ALTER TABLE toplevel_oplists ADD COLUMN oplist BYTEA;
ALTER TABLE toplevel_oplists ADD COLUMN oplist_cache BYTEA