CREATE INDEX function_arguments_most_recent
ON function_arguments
(canvas_id, tlid, timestamp DESC);

CREATE INDEX function_arguments_for_trace
ON function_arguments
(canvas_id, tlid, trace_id)
