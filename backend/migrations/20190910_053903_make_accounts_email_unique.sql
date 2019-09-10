CREATE UNIQUE INDEX idx_accounts_email_uniqueness ON accounts (email);
ALTER TABLE accounts ADD constraint emails UNIQUE USING INDEX idx_accounts_email_uniqueness
