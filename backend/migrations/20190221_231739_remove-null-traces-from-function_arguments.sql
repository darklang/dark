DELETE FROM function_arguments WHERE trace_id IS NULL;

ALTER TABLE function_arguments ALTER COLUMN trace_id SET NOT NULL
