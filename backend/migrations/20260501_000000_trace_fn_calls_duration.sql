-- Add per-call timing to trace_fn_calls. NOT NULL with default 0 so old
-- rows backfill cheaply. Function and lambda frames get real durations
-- (frame-entry to frame-exit); builtins remain at 0 since the recorder
-- only sees their synchronous storeFnResult, with no matching entry hook.

ALTER TABLE trace_fn_calls ADD COLUMN duration_ms INTEGER NOT NULL DEFAULT 0;
