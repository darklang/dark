Records of the stored\_events migration to stored\_events\_v2

Plan:
https://trello.com/c/tusw6Fab/442-migrate-storedevent-table

SQL to copying the data

```
prodclone=# insert into stored_events_v2 (canvas_id, module, path, modifier, timestamp, value, trace_id) select canvas_id, module, path, modifier, timestamp, value, trace_id from stored_events where trace_id is not null;
INSERT 0 1519487
Time: 26347.024 ms

prodclone=# insert into stored_events_v2 (canvas_id, module, path, modifier, timestamp, value, trace_id) select canvas_id, module, path, modifier, timestamp, value, gen_random_uuid() from stored_events where trace_id is null;
INSERT 0 7883196
Time: 133830.074 ms


prodclone=# select count(*) from stored_events;
 9402683
prodclone=# select count(*) from stored_events_v2;
 9402683
```
