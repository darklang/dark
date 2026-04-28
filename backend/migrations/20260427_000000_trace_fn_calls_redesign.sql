-- Redesign trace_fn_calls: every fn call AND every lambda invocation becomes
-- a row, linked via parent_call_id. Drop the old _v0 trace tables.
--
-- Also drop traces_v0's redundant `trace_id` (= `id`) and unused
-- `callgraph_tlids` (always written as ''). Rename traces_v0 → traces
-- to match the post-Oct-2025 naming convention. Inline the handler input
-- onto the trace row (it's always one var per trace).

DROP INDEX IF EXISTS idx_trace_fn_calls_trace_id;
DROP INDEX IF EXISTS idx_trace_fn_calls_fn_name;
DROP INDEX IF EXISTS idx_trace_inputs_trace_id;
DROP TABLE IF EXISTS trace_fn_calls;
DROP TABLE IF EXISTS trace_inputs_v0;
DROP TABLE IF EXISTS traces_v0;


-- One row per handler invocation. The handler input (parsed dval bound to
-- the handler's parameter — `request` for HTTP, `expression` for eval) lives
-- directly here as JSON, not in a separate one-row-per-trace table.
CREATE TABLE traces
( id               TEXT PRIMARY KEY
, canvas_id        TEXT NOT NULL
, root_tlid        INTEGER NOT NULL
, handler_desc     TEXT NOT NULL
, timestamp        TEXT NOT NULL
, input_name       TEXT NOT NULL
, input_value_json TEXT NOT NULL
);
CREATE INDEX idx_traces_canvas_id ON traces(canvas_id);


-- Every fn call and every lambda invocation gets one row. parent_call_id
-- points directly at the calling event's call_id (NULL for source-level).
-- kind discriminates fn / lambda / builtin so the renderer can pick the
-- right tag without inspecting fn_hash.
CREATE TABLE trace_fn_calls
( trace_id        TEXT NOT NULL
, call_id         TEXT NOT NULL
, parent_call_id  TEXT                            -- NULL for source-level
, kind            TEXT NOT NULL                   -- "function" | "lambda" | "builtin"
, fn_hash         TEXT                            -- callee for function/builtin
, lambda_expr_id  TEXT                            -- AST id of the lambda body
, args_json       TEXT NOT NULL                   -- JSON array of dval JSONs
, result_json     TEXT NOT NULL
, PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);
