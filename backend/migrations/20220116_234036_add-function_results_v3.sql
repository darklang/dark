CREATE TABLE IF NOT EXISTS
function_results_v3
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, tlid BIGINT NOT NULL
, fnname TEXT NOT NULL
, id BIGINT NOT NULL
, hash TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
, trace_id UUID NOT NULL
, hash_version INTEGER
);

CREATE INDEX IF NOT EXISTS
idx_function_results_v3_most_recent
ON function_results_v3
(canvas_id, trace_id, tlid, timestamp DESC)
