CREATE INDEX IF NOT EXISTS
idx_function_results_most_recent
ON function_results
(canvas_id, tlid, fnname, id, hash, timestamp DESC)
