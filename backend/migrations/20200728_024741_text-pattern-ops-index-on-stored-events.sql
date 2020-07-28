CREATE INDEX IF NOT EXISTS
idx_stored_events_v2_most_recent_with_text
ON stored_events_v2
(canvas_id, module, path text_pattern_ops, modifier, "timestamp" DESC)