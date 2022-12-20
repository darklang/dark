-- We don't use IF NOT EXISTS here because if they exist something is wrong and we
-- don't want to create the indexes as the table will be non-empty - we'd rather have
-- the error.

-- Keep track of what traces are available for which handler/function/etc
CREATE TABLE
traces_v0
( canvas_id UUID NOT NULL
-- the handler's (or for a function's default trace, the function's) TLID (used to
-- store the trace data in Cloud Storage)
, root_tlid BIGINT NOT NULL
, trace_id UUID NOT NULL
, callgraph_tlids BIGINT[] NOT NULL -- functions called during the trace
, PRIMARY KEY (canvas_id, root_tlid, trace_id)
)