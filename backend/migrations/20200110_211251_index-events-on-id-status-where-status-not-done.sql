create index if not exists idx_events_for_dequeue on events (status, id) where status <> 'done'
