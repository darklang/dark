UPDATE accounts
SET username = 'sample'
WHERE username = 'demo'
AND NOT EXISTS (SELECT 1 from accounts WHERE username = 'sample')
