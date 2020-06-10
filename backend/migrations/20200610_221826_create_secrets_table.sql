CREATE TABLE IF NOT EXISTS
secrets
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, secret_name VARCHAR(255) NOT NULL
, secret_value TEXT NOT NULL
, secret_version INT NOT NULL DEFAULT 0
, created_at TIMESTAMP NOT NULL DEFAULT NOW()
, PRIMARY KEY (canvas_id, secret_name, secret_version))
