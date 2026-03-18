-- Add trace metadata columns to traces_v0
ALTER TABLE traces_v0 ADD COLUMN handler_desc TEXT NOT NULL DEFAULT '';
ALTER TABLE traces_v0 ADD COLUMN timestamp TEXT NOT NULL DEFAULT '';


CREATE TABLE IF NOT EXISTS trace_inputs_v0
( trace_id TEXT NOT NULL
, name TEXT NOT NULL
, value_json TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS trace_fn_results_v0
( trace_id TEXT NOT NULL
, fn_name TEXT NOT NULL
, args_hash TEXT NOT NULL
, hash_version INTEGER NOT NULL DEFAULT 0
, result_json TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS trace_fn_arguments_v0
( trace_id TEXT NOT NULL
, fn_name TEXT NOT NULL
, args_hash TEXT NOT NULL
, args_json TEXT NOT NULL DEFAULT '[]'
);

CREATE INDEX IF NOT EXISTS idx_trace_inputs_trace_id ON trace_inputs_v0(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_results_trace_id ON trace_fn_results_v0(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_results_fn_name ON trace_fn_results_v0(fn_name);
CREATE INDEX IF NOT EXISTS idx_trace_fn_results_lookup
  ON trace_fn_results_v0(trace_id, fn_name, args_hash);
CREATE INDEX IF NOT EXISTS idx_trace_fn_arguments_trace_id ON trace_fn_arguments_v0(trace_id);
