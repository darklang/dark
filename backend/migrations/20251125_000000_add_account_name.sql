-- Add name column to accounts_v0
ALTER TABLE accounts_v0 ADD COLUMN name TEXT NOT NULL DEFAULT '';
