CREATE INDEX IF NOT EXISTS
idx_user_data_fetch
ON user_data
(account_id, canvas_id, table_tlid, user_version, dark_version)
