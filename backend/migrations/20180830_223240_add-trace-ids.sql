ALTER TABLE stored_events
ADD COLUMN trace_id UUID; -- nullable for now

ALTER TABLE function_arguments
ADD COLUMN trace_id UUID; -- nullable for now

ALTER TABLE function_results
ADD COLUMN trace_id UUID -- nullable for now
