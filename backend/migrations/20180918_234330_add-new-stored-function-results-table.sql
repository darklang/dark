CREATE TABLE IF NOT EXISTS
function_results_v2
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, tlid BIGINT NOT NULL
, fnname TEXT NOT NULL
, id BIGINT NOT NULL
, hash TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
, trace_id UUID NOT NULL
);

CREATE INDEX IF NOT EXISTS
idx_function_results_v2_most_recent
ON function_results_v2
(canvas_id, trace_id, tlid, timestamp DESC)
