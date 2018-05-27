CREATE INDEX IF NOT EXISTS
idx_dequeue
ON events
(account_id, canvas_id, space, name, status, delay_until, id)
