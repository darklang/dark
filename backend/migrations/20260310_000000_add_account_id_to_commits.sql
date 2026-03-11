-- Add committer attribution to commits
ALTER TABLE commits ADD COLUMN account_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001' REFERENCES accounts_v0(id);
