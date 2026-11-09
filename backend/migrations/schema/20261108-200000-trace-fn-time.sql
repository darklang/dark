-- What a recorded run spent in each package function it went through (`trace_fns`): its calls,
-- the wall clock inside it, and its slowest call. `00-initial-schema.sql` explains the columns.
ALTER TABLE trace_fns ADD COLUMN calls INTEGER NOT NULL DEFAULT 0;
ALTER TABLE trace_fns ADD COLUMN total_us INTEGER NOT NULL DEFAULT 0;
ALTER TABLE trace_fns ADD COLUMN slowest_us INTEGER NOT NULL DEFAULT 0;
