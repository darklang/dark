UPDATE stored_events
SET trace_id = gen_random_uuid()
WHERE trace_id IS NULL;

ALTER TABLE stored_events
ALTER COLUMN trace_id SET NOT NULL

