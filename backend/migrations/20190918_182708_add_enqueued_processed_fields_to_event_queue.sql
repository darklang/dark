ALTER TABLE events ADD COLUMN enqueued_at timestamp with time zone;
ALTER TABLE events ADD COLUMN last_processed_at timestamp with time zone
