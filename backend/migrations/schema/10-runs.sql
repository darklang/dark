-- A trace IS a run, and this file is everything the run half of that needs that `08-traces.sql`
-- cannot hold. That file has merged, so it is frozen (`AGENTS.md`, "Changing the schema"): the
-- COLUMNS this PR adds to `traces` and `trace_fn_calls` therefore come from steps in
-- `LibDB/Releases.fs`, which is the only thing that can widen a table that already exists. What
-- CAN live here is a new table and any index, so both are here.

--------------------
-- Which functions a run went through: names only
--------------------

-- The shipped recording level keeps only the impure calls, so nothing in `trace_fn_calls` says
-- that a run passed through `MyApp.Orders.route`. This is what `dark traces calls <fn>` reads:
-- one row per (run, function), no arguments, no results, a few hundred bytes for a normal run.
CREATE TABLE IF NOT EXISTS trace_fns (
  trace_id TEXT NOT NULL,
  fn_name TEXT NOT NULL,
  PRIMARY KEY (trace_id, fn_name)
);
CREATE INDEX IF NOT EXISTS idx_trace_fns_fn_name ON trace_fns(fn_name);


--------------------
-- Indexes for the run verbs
--------------------

-- Indexes are applied in a pass of their own, after the release steps, so one may name a column
-- a step has just added (`Releases.applySchemaIndexes`). Every listing and retention's scan order
-- by `timestamp DESC`; `status` is how the CLI finds the runs that can be resumed.
CREATE INDEX IF NOT EXISTS idx_traces_timestamp ON traces(timestamp);
CREATE INDEX IF NOT EXISTS idx_traces_status    ON traces(status);
