-- Merge trace_fn_results_v0 + trace_fn_arguments_v0 (from 20260318_000000)
-- into one table.
--
-- The pre-merge split looked like "args table + results table joined by
-- args_hash," but the two tables were 1:1 in cardinality and keyed the
-- same way. args_hash was written and indexed but never queried: no read
-- path joined on it, no write path deduped on it. So the split bought
-- nothing - cost 2× writes plus a JOIN on every read. One row per fn call
-- is what the viewer actually consumes.


DROP INDEX IF EXISTS idx_trace_fn_results_trace_id;
DROP INDEX IF EXISTS idx_trace_fn_results_fn_name;
DROP INDEX IF EXISTS idx_trace_fn_results_lookup;
DROP INDEX IF EXISTS idx_trace_fn_arguments_trace_id;
DROP TABLE IF EXISTS trace_fn_arguments_v0;
DROP TABLE IF EXISTS trace_fn_results_v0;

CREATE TABLE trace_fn_calls_v0
( trace_id TEXT NOT NULL
, caller_kind TEXT NOT NULL   -- "source" | "fn_body" | "lambda_body"
, caller TEXT NOT NULL        -- containing fn pretty-name (empty for source)
, fn_kind TEXT NOT NULL       -- "builtin" | "package"
, fn_name TEXT NOT NULL
, args_json TEXT NOT NULL
, result_json TEXT NOT NULL
);

CREATE INDEX idx_trace_fn_calls_trace_id ON trace_fn_calls_v0(trace_id);
CREATE INDEX idx_trace_fn_calls_fn_name ON trace_fn_calls_v0(fn_name);
