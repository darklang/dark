CREATE TABLE IF NOT EXISTS
function_arguments
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, tlid BIGINT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, arguments_json TEXT NOT NULL
)
