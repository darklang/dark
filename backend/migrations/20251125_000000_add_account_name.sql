-- Add name column to accounts_v0
ALTER TABLE accounts_v0 ADD COLUMN name TEXT NOT NULL DEFAULT '';
-- Enforce unique usernames
CREATE UNIQUE INDEX IF NOT EXISTS accounts_name_unique ON accounts_v0(name);
