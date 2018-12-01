DROP TRIGGER IF EXISTS set_account_timestamp
ON accounts;

CREATE TRIGGER set_account_timestamp
BEFORE UPDATE ON accounts
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
