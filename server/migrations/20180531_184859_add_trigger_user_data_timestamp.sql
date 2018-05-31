DROP TRIGGER IF EXISTS set_user_data_timestamp
ON user_data;

CREATE TRIGGER set_user_data_timestamp
BEFORE UPDATE ON user_data
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
