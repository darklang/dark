-- Restore the `accounts_v0` table + commits/traces account_id columns
-- that were dropped in this branch's account-removal pass. Coworker
-- re-flagged: accounts (the developer behind a commit) belong;
-- canvas / app / domain were the right thing to drop, accounts
-- weren't.
--
-- Single-instance Dark today means there's effectively one account
-- per process, but tracking who authored a commit is still load-
-- bearing for audit + the future multi-tenant story. Schema-level
-- accounts are cheap and cleanly separated from canvas/app/domain
-- (which stays gone).

CREATE TABLE IF NOT EXISTS accounts_v0
( id TEXT PRIMARY KEY
, name TEXT NOT NULL UNIQUE
, created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Seed: pre-allocated UUIDs so Dark code can reference them by ID.
-- These IDs are part of the API; don't rotate.
INSERT OR IGNORE INTO accounts_v0 (id, name) VALUES
  ('00000000-0000-0000-0000-000000000001', 'Darklang'),
  ('00000000-0000-0000-0000-000000000002', 'Stachu'),
  ('00000000-0000-0000-0000-000000000003', 'Paul'),
  ('00000000-0000-0000-0000-000000000004', 'Feriel');

-- Commits: who authored. Default to Darklang so the migration is
-- safe on existing rows (CLI uses Darklang as the default account).
ALTER TABLE commits
  ADD COLUMN account_id TEXT NOT NULL
  DEFAULT '00000000-0000-0000-0000-000000000001'
  REFERENCES accounts_v0(id);

-- Traces: who triggered. Nullable — existing trace rows pre-date
-- account attribution. New rows always carry the account.
ALTER TABLE traces ADD COLUMN account_id TEXT REFERENCES accounts_v0(id);
