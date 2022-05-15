--#[no_tx]

-- We want to use this index to:
-- 1) count the number of items in this queue, so it's important that the entire
-- search term is in the index or it will need to hit disk. This is true even though
-- the module rarely changes
-- 2) fetch the indexes for all items we're unpausing. This is rare so it's fine to
create index concurrently if not exists idx_eventsv2_count
on events_v2 (canvas_id, module, name)