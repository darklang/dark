CREATE INDEX IF NOT EXISTS
idx_stored_events_most_recent
ON stored_events
(canvas_id, module, path, modifier, timestamp DESC)
