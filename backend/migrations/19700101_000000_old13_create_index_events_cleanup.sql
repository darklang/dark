CREATE INDEX IF NOT EXISTS
idx_cleanup
ON events
(dequeued_by, status)
