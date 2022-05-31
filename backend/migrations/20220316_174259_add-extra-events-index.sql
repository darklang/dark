--#[no_tx]
create index concurrently if not exists idx_events_for_dequeue2 on events (status, id) where status = 'scheduled'