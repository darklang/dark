Recording the SQL run by hand.

```
UPDATE stored_events
SET trace_id = gen_random_uuid()
WHERE trace_id IS NULL
```

https://trello.com/c/LpiXeuR9/445-practice-run-at-all-the-data-migration

This is required by the weird ordering in the stored_event migration.

We want to keep 10 stored_eventsper toplevel. The cron cleanup script
(https://github.com/darklang/dark/pull/204) does that but completely
ignores items without a traceid.

The data migration script from the Stored_event migration then copies
everything without a traceid over. As a result, it copies almost everything,
and we save basically no data. We could do the cleanup again, but we'd have
lost our opportunity to vacuum.

Instead, if we add traceids first, then we can do the cleanup on
stored_events. Then we'll only copy what we need to stored_events_v2.

