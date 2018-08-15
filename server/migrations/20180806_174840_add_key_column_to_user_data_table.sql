ALTER TABLE user_data
ADD COLUMN key TEXT NOT NULL DEFAULT '';

UPDATE user_data SET key = id::text;

ALTER TABLE user_data ALTER COLUMN key DROP DEFAULT;

ALTER TABLE user_data DROP CONSTRAINT user_data_pkey;
ALTER TABLE user_data ADD PRIMARY KEY (id);

CREATE UNIQUE INDEX idx_user_data_row_uniqueness
ON user_data
(account_id, canvas_id, table_tlid, dark_version, user_version, key);

ALTER TABLE user_data ADD constraint user_data_key_uniq UNIQUE USING INDEX idx_user_data_row_uniqueness

