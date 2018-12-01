CREATE TABLE IF NOT EXISTS
stored_events_v2
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, module TEXT NOT NULL
, path TEXT NOT NULL
, modifier TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
, trace_id UUID NOT NULL
);
CREATE INDEX IF NOT EXISTS
idx_stored_events_v2_most_recent
ON stored_events_v2
(canvas_id, module, path, modifier, timestamp DESC)


