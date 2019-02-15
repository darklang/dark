CREATE UNIQUE INDEX stored_events_v2_traceid_uniq
ON stored_events_v2
(canvas_id, trace_id);

ALTER TABLE stored_events_v2 ADD constraint stored_events_v2_traceid_uniq UNIQUE USING INDEX stored_events_v2_traceid_uniq
