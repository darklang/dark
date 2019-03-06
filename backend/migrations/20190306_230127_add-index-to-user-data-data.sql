CREATE INDEX IF NOT EXISTS idx_user_data_gin_data
ON user_data
USING GIN
(data jsonb_path_ops)
