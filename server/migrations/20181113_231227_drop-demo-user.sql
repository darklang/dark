-- we can't delete this account without breaking canvases_account_id_fkey, but
-- we can make it impossible to log in
UPDATE accounts
SET password = ''
WHERE username = 'demo';
