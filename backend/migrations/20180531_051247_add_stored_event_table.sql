CREATE TABLE IF NOT EXISTS
stored_events
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, module TEXT NOT NULL
, path TEXT NOT NULL
, modifier TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
)

