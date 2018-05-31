CREATE TABLE IF NOT EXISTS
user_data
(id UUID NOT NULL
, account_id UUID REFERENCES accounts(id) NOT NULL
, canvas_id UUID REFERENCES canvases(id) NOT NULL
, table_tlid BIGINT NOT NULL
, user_version INT NOT NULL
, dark_version INT NOT NULL
, data JSONB NOT NULL
, created_at TIMESTAMP NOT NULL DEFAULT NOW()
, updated_at TIMESTAMP NOT NULL DEFAULT NOW()
, PRIMARY KEY(id, user_version, dark_version)
)
