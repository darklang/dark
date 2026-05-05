-- note that the system_migration_v0 table is also crated by Migrations.fs
CREATE TABLE IF NOT EXISTS
system_migrations_v0
( name TEXT PRIMARY KEY
, execution_date TEXT NOT NULL -- timestamp
, sql TEXT NOT NULL
);


--------------------
-- Stuff that belongs in "user space"
--------------------
-- Note: `account_id` is the per-test / per-install scope key. There
-- is no `accounts_v0` foreign key — in CLI mode the value is just
-- Guid.Empty (no account); in tests it's a fresh Guid per test for
-- isolation. The accounts_v0 table (later migration) is independent
-- and seeded with the canonical user IDs.

-- User K/V DBs
CREATE TABLE IF NOT EXISTS
user_data_v0
( id TEXT PRIMARY KEY
, account_id TEXT NOT NULL
, table_tlid INTEGER NOT NULL
, user_version INTEGER NOT NULL
, dark_version INTEGER NOT NULL
, data TEXT NOT NULL -- JSON stored as text
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
, updated_at TEXT NOT NULL DEFAULT (datetime('now'))
, key TEXT NOT NULL
, UNIQUE (account_id, table_tlid, dark_version, user_version, key)
);

CREATE INDEX IF NOT EXISTS
idx_user_data_fetch
ON user_data_v0
(account_id, table_tlid, user_version, dark_version);

CREATE INDEX IF NOT EXISTS
idx_user_data_current_data_for_tlid
ON user_data_v0
(user_version, dark_version, account_id, table_tlid);

-- No GIN index equivalent in SQLite
CREATE INDEX IF NOT EXISTS
idx_user_data_json
ON user_data_v0
(data);


-- HTTP Handlers
-- TODO: extract out table of http handlers from toplevels_v0



-- Top-levels
-- TODO split this into a few tables (dbs, handlers, etc)
CREATE TABLE IF NOT EXISTS
toplevels_v0
( account_id TEXT NOT NULL
, tlid INTEGER NOT NULL
, digest CHAR(32) NOT NULL
, tipe TEXT NOT NULL CHECK (tipe IN ('db', 'handler'))
, name TEXT /* handlers only - used for http lookups */
, module TEXT /* handlers only */
, modifier TEXT /* handlers only */
, updated_at TEXT NOT NULL DEFAULT (datetime('now'))
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
, deleted INTEGER NOT NULL CHECK (deleted IN (0,1))
, data BLOB NOT NULL
, PRIMARY KEY (account_id, tlid)
);

-- Traces
CREATE TABLE IF NOT EXISTS
traces_v0
( id TEXT PRIMARY KEY
, trace_id TEXT NOT NULL -- why do we need this _and_ `id`?
, account_id TEXT NOT NULL
-- the handler's (or for a function's default trace, the function's) TLID
--   (used to store the trace data in Cloud Storage)
-- TODO consider using a different mechanism here - fns might not have tlids...
--   why wouldn't we use the `id` instead? length?
, root_tlid INTEGER NOT NULL
, callgraph_tlids TEXT NOT NULL -- functions called during the trace
);