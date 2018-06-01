CREATE INDEX IF NOT EXISTS
idx_user_data_current_data_for_tlid
ON user_data
(user_version, dark_version, table_tlid)
