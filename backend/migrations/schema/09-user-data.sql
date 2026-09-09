-- The user's own program state: DB values, toplevels, saved scripts and config. Nothing here is
-- derived from the op log.

--------------------
-- User-space (apps)
--------------------
-- Single-instance Dark: one DB per process, no per-scope key. Test isolation comes from wiping these
-- tables between tests (`TestUtils.initializeTestCanvas` truncates), not from scoping rows.

-- User K/V DBs (the runtime backing for `Stdlib.DB.*`).
CREATE TABLE IF NOT EXISTS user_data_v0 (
  id TEXT PRIMARY KEY,
  table_tlid INTEGER NOT NULL,
  user_version INTEGER NOT NULL,
  dark_version INTEGER NOT NULL,
  data TEXT NOT NULL,                       -- JSON stored as text
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  key TEXT NOT NULL,
  UNIQUE (table_tlid, dark_version, user_version, key)
);
CREATE INDEX IF NOT EXISTS idx_user_data_fetch
  ON user_data_v0 (table_tlid, user_version, dark_version);
CREATE INDEX IF NOT EXISTS idx_user_data_current_data_for_tlid
  ON user_data_v0 (user_version, dark_version, table_tlid);
-- (No GIN index equivalent in SQLite.)
CREATE INDEX IF NOT EXISTS idx_user_data_json ON user_data_v0 (data);


-- Top-levels (legacy). TODO split into a few tables (dbs, handlers, etc).
CREATE TABLE IF NOT EXISTS toplevels_v0 (
  tlid INTEGER NOT NULL PRIMARY KEY,
  digest CHAR(32) NOT NULL,
  tipe TEXT NOT NULL CHECK (tipe IN ('db', 'handler')),
  name TEXT,                                -- handlers only, used for HTTP lookups
  module TEXT,                              -- handlers only
  modifier TEXT,                            -- handlers only
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  deleted INTEGER NOT NULL CHECK (deleted IN (0, 1)),
  data BLOB NOT NULL
);


-- User-saved CLI scripts (managed via `dark scripts ...`).
CREATE TABLE IF NOT EXISTS scripts_v0 (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  text TEXT NOT NULL
);

-- Mutable, per-install local config: the CLI entry-point pointer (`entry_point`) plus per-user
-- settings. Deliberately NOT content-addressed and NOT synced -- sync ships ops, never this table.
CREATE TABLE IF NOT EXISTS config_v0 (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
