CREATE TABLE IF NOT EXISTS
secrets
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, secret_name VARCHAR(255) NOT NULL
, secret_value TEXT NOT NULL
, value_version INT NOT NULL
, PRIMARY KEY (canvas_id, secret_name, value_version))
