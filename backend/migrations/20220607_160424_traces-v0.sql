-- We don't use IF NOT EXISTS here because if they exist something is wrong and we
-- don't want to create the indexes as the table will be non-empty - we'd rather have
-- the error.

-- Keep track of what traces are available for which handler/function/etc
CREATE TABLE
traces_v0
( canvas_id UUID NOT NULL
, tlid BIGINT NOT NULL
, trace_id UUID NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
-- allows looking up handler by trace
, PRIMARY KEY (canvas_id, tlid, trace_id)
);

-- Look up by trace (allows for fast deletion)
CREATE INDEX
idx_traces_trace
ON traces_v0
(canvas_id, trace_id)
